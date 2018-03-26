
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module Neovim.Ghcid.Simple.Plugin where

import           Control.Arrow                          (second)
import           Control.Concurrent.STM
import           Control.Lens                           (makeLenses)
import           Control.Lens.Operators
import           Control.Monad                          (forM_, unless)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Catch                    (SomeException, catch, throwM)
import           Control.Monad.Extra                    (whenJust)
import qualified Data.ByteString.Char8                  as B
import           Data.List                              (find, isSuffixOf)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as M
import           Data.Maybe                             (isJust)
import           Data.Set                               (Set)
import qualified Data.Set                               as S
import           Distribution.PackageDescription        (GenericPackageDescription (..))
import           Distribution.PackageDescription.Parse  (readGenericPackageDescription)
import           Distribution.Types.UnqualComponentName (unUnqualComponentName)
import           Distribution.Verbosity                 (silent)
import           Language.Haskell.Ghcid                 as Ghcid
import           Neovim
import           Neovim.BuildTool
import qualified Neovim.Quickfix                        as Q
import           Neovim.User.Choice                     (askForIndex)
import           System.Directory                       (canonicalizePath,
                                                         getDirectoryContents)
import           System.FilePath.Posix                  (isRelative, (</>))
import           System.IO.Unsafe                       (unsafePerformIO)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Target = Target {
    _targetID        :: !Int
  , _targetName      :: !String
  , _targetCabalFile :: !FilePath
  , _targetGhci      :: !Ghci
  , _targetFiles     :: !(Set (FilePath, String))
  }
makeLenses ''Target

filesInTarget :: Target -> Set FilePath
filesInTarget t = S.map snd (t^.targetFiles)

moduleNamesInTarget :: Target -> Set FilePath
moduleNamesInTarget t = S.map fst (t^.targetFiles)

type NeovimGhcid = Neovim GhcidEnv

data GhcidEnv = GhcidEnv
  { targets :: !(TVar (Map Int Target))
  , unique  :: !(TVar Int)
  }
makeLenses ''GhcidEnv

initialEnv :: GhcidEnv
initialEnv = GhcidEnv
  { targets = unsafePerformIO (newTVarIO M.empty)
  , unique  = unsafePerformIO (newTVarIO 0)
  }

genUnique :: NeovimGhcid Int
genUnique = do
  v <- asks unique
  liftIO $ atomically $ do
    modifyTVar' v (+1)
    readTVar v


-- Util
--------
readTVar_ :: MonadIO m => TVar a -> m a
readTVar_ = liftIO . readTVarIO

writeTVar_ :: MonadIO m => TVar a -> a -> m ()
writeTVar_ var val = liftIO $ atomically $ writeTVar var val

swapTVar_ :: MonadIO m => TVar a -> a -> m a
swapTVar_ var val = liftIO $ atomically $ swapTVar var val

modifyTVar_ :: MonadIO m => TVar a -> (a -> a) -> m ()
modifyTVar_ var f = liftIO $ atomically $ modifyTVar' var f

use' :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> m a
use' getter = readTVar_ =<< asks getter

uses' :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> (a -> r) -> m r
uses' getter f = f <$> use' getter

(.==) :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> a -> m ()
getter .== x = do
  v <- asks getter
  writeTVar_ v x
(%==) :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> (a -> a) -> m ()
getter %== f = do
  v <- asks getter
  modifyTVar_ v f

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

ghcidCheck :: CommandArguments -> Maybe String -> NeovimGhcid ()
ghcidCheck ca mFile = do
    file <- maybe nvimCurrentFile return mFile
    mtgt <- lookupFile file >>= \case
      Just tgt -> Just <$> reloadTarget tgt
      Nothing  -> newGhci default'
    whenJust mtgt (warnIfNotLoaded file)
  where
    default' = bang ca == Just True
    warnIfNotLoaded file tgt =
      unless (file `elem` filesInTarget tgt) $
        vim_report_error' $
          "Warning: " ++ file ++ " is not loaded in this target\n"

ghcidShowStatus :: CommandArguments -> NeovimGhcid ()
ghcidShowStatus _ = do
  tgts <- use' targets
  if null tgts
    then vim_report_error' "no ghci process is running"
    else vim_report_error' $ init $ unlines $ concat
            [ t^.targetName : map ("  "++) (S.toList (moduleNamesInTarget t) <&> id)
            | (_,t) <- M.toList tgts ]

ghcidStopAll :: CommandArguments -> NeovimGhcid ()
ghcidStopAll _ = do
  tgts <- swapTVar_ `flip` M.empty =<< asks targets
  liftIO $ forM_ tgts $ \t -> stopGhci (t^.targetGhci)

-------------------------------------------------------------------------------
-- Target
-------------------------------------------------------------------------------

lookupFile :: FilePath -> NeovimGhcid (Maybe Target)
lookupFile f = uses' targets $ find (`hasFile` f)

lookupTgtname :: String -> NeovimGhcid (Maybe Target)
lookupTgtname tgtName = uses' targets $ find ((tgtName==) . (^.targetName))

hasFile :: Target -> FilePath -> Bool
hasFile t f = f `S.member` filesInTarget t

addTarget :: Target -> NeovimGhcid ()
addTarget p = targets %== M.insert (p^.targetID) p

ghciTarget :: Ghci -> Directory -> FilePath -> String -> NeovimGhcid Target
ghciTarget g (Directory rootDir) cabal name = do
    files <- liftIO $ map (second mkAbosolute) <$> showModules g
    id'   <- genUnique
    return $ Target id' name cabal g (S.fromList files)
  where
    -- When files are in rootDir, 'showModules' returns relative paths.
    -- at least with ghc-8.2.1
    -- TODO テスト
    mkAbosolute f
      | isRelative f = rootDir </> f
      | otherwise    = f

reloadTarget :: Target -> NeovimGhcid Target
reloadTarget tgt@Target{_targetGhci=g} = do
  setLoadsQFlist =<< liftIO (reload g)
  files <- S.fromList <$> liftIO (showModules g)
  let tgt' = tgt & targetFiles %~ S.union files
  addTarget tgt'
  return tgt'

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

newGhci :: Bool -> NeovimGhcid (Maybe Target)
newGhci default' = do
  dir <- Directory <$> nvimCurrentFileDir
  guessProjectSettings (thisAndParentDirectories dir) >>= askTarget default' >>= \case
    Just (root@(Directory rootDir), cabal, tgtName, cmd) ->
      lookupTgtname tgtName >>= \case
        Nothing -> do
          m <- liftIO $ (Right <$> startGhci cmd (Just rootDir) (\_ _ -> return ()))
                    `catch` \(e :: SomeException) -> return (Left e)
          case m of
            Right (g, loads) -> do setLoadsQFlist loads
                                   tgt <- ghciTarget g root cabal tgtName
                                   addTarget tgt
                                   return $ Just tgt
            Left e -> vim_report_error' (show e) >> throwM e

        Just tgt -> Just <$> reloadTarget tgt

    _ -> vim_report_error' "canceled" >> return Nothing


-- Returns (project root, cabal file, target name, command to be passed to ghcid)
askTarget :: Bool -> Maybe (BuildTool, Directory)
          -> NeovimGhcid (Maybe (Directory, FilePath, String, String))
askTarget default' (Just (Stack, dir))   = askTarget' default' "stack ghci" dir
askTarget default' (Just (Cabal{}, dir)) = askTarget' default' "cabal repl" dir
askTarget _ _ = vim_report_error' msg >> err (pretty msg)
  where msg = "*.cabal not found"

askTarget' :: Bool -> String -> Directory
           -> NeovimGhcid (Maybe (Directory, FilePath, String, String))
askTarget' default' baseCmd dir@(Directory dirPath) = do
  cabal <- getCabalFile dirPath
  let cabalPath   = dirPath </> cabal
      packageName = take (length cabal - 6) cabal
      defaultCfg  = Just (dir, cabalPath, packageName++"-default", baseCmd)
  if default'
    then return defaultCfg
    else do
      description <- liftIO $ readGenericPackageDescription silent cabalPath
      let candidates  = enumCandidates description packageName
      x <- askForIndex $ map (ObjectString . B.pack)
                [ "("++show n++") " ++ baseCmd ++ " " ++ c
                |  (n,c) <- zip [1::Int ..] ("" : candidates) ]
      case x of
        Just 1 -> return defaultCfg
        Just n -> return $ Just (dir, cabalPath, tgtName, cmd)
            where tgtName = candidates!!(n-2)
                  cmd     = baseCmd ++ " " ++ tgtName
        Nothing -> return Nothing

enumCandidates :: GenericPackageDescription -> String -> [String]
enumCandidates GenericPackageDescription{..} packageName = concat [libs, exes, tests]
  where
    libs  = [ packageName ++ ":lib"                                 | isJust condLibrary          ]
    exes  = [ packageName ++ ":exe:"  ++ unUnqualComponentName exe  | (exe,_)  <- condExecutables ]
    tests = [ packageName ++ ":test:" ++ unUnqualComponentName test | (test,_) <- condTestSuites  ]

getCabalFile :: FilePath -> Neovim st FilePath
getCabalFile dir = do
  files <- liftIO $ getDirectoryContents dir
  case find (".cabal" `isSuffixOf`) files of
    Just f -> return f
    Nothing -> err "*.cabal not found"

-------------------------------------------------------------------------------
-- Quickfix
-------------------------------------------------------------------------------

-- | Set error/warnings in Quickfix list
setLoadsQFlist :: [Load] -> Neovim st ()
setLoadsQFlist loads = do
  Q.setqflist qfs Q.Replace
  if null qfs
    then void $ vim_command "cclose | echo \"ghcid-nvim: no error found\""
    else void $ vim_command "botright copen"
  where
    qfs = concatMap toQFItems loads'
    loads' = case filter isError loads of
               [] -> ignoreAlexWarning loads
               ls -> ls
    isError l = isMessage l && loadSeverity l==Ghcid.Error
    isMessage Message{} = True
    isMessage Loading{} = False
    -- "templates/wrapper.hs" is generated by lexer generator `alex`,
    -- which includes some warnings
    ignoreAlexWarning = filter p
      where p (Message _ file _ _) = not $ "templates/wrapper.hs" `isSuffixOf` file
            p Loading{} = False

-- | Convert Ghcid.Load to Quickfix Items
toQFItems :: Load -> [Q.QuickfixListItem String]
toQFItems Loading{} = []
toQFItems (Message severity file (lnum,col) msgs) = header : map f msgs
  where
    header = Q.QFItem {
        Q.bufOrFile     = Right file
      , Q.lnumOrPattern = Left lnum
      , Q.col           = Q.ByteIndexColumn col
      , Q.nr            = Nothing
      , Q.text          = ""
      , Q.errorType     = toQFSeverity severity
      }
    f msg = Q.QFItem {
        Q.bufOrFile     = Right ""
      , Q.lnumOrPattern = Right ""
      , Q.col           = Q.NoColumn
      , Q.nr            = Nothing
      , Q.text          = msg
      , Q.errorType     = Q.Warning
      }

    toQFSeverity Ghcid.Error   = Q.Error
    toQFSeverity Ghcid.Warning = Q.Warning

-------------------------------------------------------------------------------
-- Neovim Util
-------------------------------------------------------------------------------

-- | an alternative to `vim_out_write'` which doesn't work for me :(
nvimEcho :: String -> Neovim st ()
nvimEcho s = void $ vim_command_output $ "echom " ++ show s

-- | return the canonical path of current working directory

--   `canonicalizePath` is necessary because nvim can return
--   a not canonical path such as "/foo//bar" ('/' duplicated)
nvimCWD :: Neovim st FilePath
nvimCWD = liftIO . canonicalizePath =<<
  errOnInvalidResult (vim_call_function "getcwd" [])

-- | return the canonical path of the current file's directory
nvimCurrentFileDir :: Neovim st FilePath
nvimCurrentFileDir = liftIO . canonicalizePath =<<
  errOnInvalidResult (vim_call_function "expand" [ObjectString "%:p:h"])

-- | return the canonical path of the current file
nvimCurrentFile :: Neovim st FilePath
nvimCurrentFile = liftIO . canonicalizePath =<<
  errOnInvalidResult (vim_call_function "expand" [ObjectString "%:p"])

