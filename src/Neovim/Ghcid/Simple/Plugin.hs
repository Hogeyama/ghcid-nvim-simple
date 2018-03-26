
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Neovim.Ghcid.Simple.Plugin where

import           Control.Arrow                          (second)
import           Control.Lens                           (makeLenses, use, uses,
                                                         view)
import           Control.Lens.Operators
import           Control.Monad                          (forM_, unless)
import           Control.Monad.Catch                    (SomeException, catch,
                                                         throwM)
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

data GhcidState = GhcidState {
    _targets :: !(Map Int Target)
  , _unique  :: !Int
  }
makeLenses ''GhcidState

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

ghcidCheck :: CommandArguments -> Maybe String -> Neovim r GhcidState ()
ghcidCheck ca mFile = do
    file <- maybe nvimCurrentFile return mFile
    mtgt <- lookupFile file >>= \case
      Just tgt -> Just <$> reloadTarget tgt
      Nothing  -> newGhci default'
    whenJust mtgt (warnIfNotLoaded file)
  where
    default' = bang ca == Just True
    warnIfNotLoaded file tgt = do
      unless (file `elem` filesInTarget tgt) $
        vim_report_error' $
          "Warning: " ++ file ++ " is not loaded in this target\n"

ghcidShowStatus :: CommandArguments -> Neovim r GhcidState ()
ghcidShowStatus _ = do
  tgts <- use targets
  if null tgts
    then vim_report_error' "no ghci process is running"
    else vim_report_error' $ init $ unlines $ concat
            [ t^.targetName : map ("  "++) (S.toList (moduleNamesInTarget t) <&> id)
            | (_,t) <- M.toList tgts ]

ghcidStopAll :: CommandArguments -> Neovim r GhcidState ()
ghcidStopAll _ = do
  tgts <- use targets
  targets .= M.empty
  forM_ tgts $ \t -> do
    liftIO $ stopGhci (t^.targetGhci)

-------------------------------------------------------------------------------
-- Target
-------------------------------------------------------------------------------

lookupFile :: FilePath -> Neovim r GhcidState (Maybe Target)
lookupFile f = uses targets $ find (`hasFile` f)

lookupTgtname :: String -> Neovim r GhcidState (Maybe Target)
lookupTgtname tgtName = uses targets $ find ((tgtName==) . view targetName)

hasFile :: Target -> FilePath -> Bool
hasFile t f = f `S.member` filesInTarget t

addTarget :: Target -> Neovim r GhcidState ()
addTarget p = targets %= M.insert (p^.targetID) p

ghciTarget :: Ghci -> Directory -> FilePath -> String -> Neovim r GhcidState Target
ghciTarget g (Directory rootDir) cabal name = do
    files <- liftIO $ map (second mkAbosolute) <$> showModules g
    id'   <- unique <+= 1
    return $ Target id' name cabal g (S.fromList files)
  where
    -- When files are in rootDir, 'showModules' returns relative paths.
    -- at least with ghc-8.2.1
    -- TODO テスト
    mkAbosolute f
      | isRelative f = rootDir </> f
      | otherwise    = f

reloadTarget :: Target -> Neovim r GhcidState Target
reloadTarget tgt@Target{_targetGhci=g} = do
  setLoadsQFlist =<< liftIO (reload g)
  files <- S.fromList <$> liftIO (showModules g)
  let tgt' = tgt & targetFiles %~ S.union files
  addTarget tgt'
  return tgt'

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

newGhci :: Bool -> Neovim r GhcidState (Maybe Target)
newGhci default' = do
  dir <- Directory <$> nvimCurrentFileDir
  guessProjectSettings (thisAndParentDirectories dir) >>= askTarget default' >>= \case
    Just (root@(Directory rootDir), cabal, tgtName, cmd) ->
      lookupTgtname tgtName >>= \case
        Nothing -> do
          (g, loads)  <- liftIO (startGhci cmd (Just rootDir) (\_ _ -> return ()))
                            `catch` \(e :: SomeException) -> do
                              vim_report_error' (show e)
                              throwM e
          setLoadsQFlist loads
          tgt <- ghciTarget g root cabal tgtName
          addTarget tgt
          return $ Just tgt

        Just tgt -> Just <$> reloadTarget tgt

    _ -> vim_report_error' "canceled" >> return Nothing


-- Returns (project root, cabal file, target name, command to be passed to ghcid)
askTarget :: Bool -> Maybe (BuildTool, Directory)
          -> Neovim r GhcidState (Maybe (Directory, FilePath, String, String))
askTarget default' (Just (Stack, dir))   = askTarget' default' "stack ghci" dir
askTarget default' (Just (Cabal{}, dir)) = askTarget' default' "cabal repl" dir
askTarget _ _ = vim_report_error' msg >> err msg
  where msg = "*.cabal not found"

askTarget' :: Bool -> String -> Directory
           -> Neovim r GhcidState (Maybe (Directory, FilePath, String, String))
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

getCabalFile :: FilePath -> Neovim r st FilePath
getCabalFile dir = do
  files <- liftIO $ getDirectoryContents dir
  return (find (".cabal" `isSuffixOf`) files)
            `orThrow` ("*.cabal not found" :: String)

-------------------------------------------------------------------------------
-- Quickfix
-------------------------------------------------------------------------------

-- | Set error/warnings in Quickfix list
setLoadsQFlist :: [Load] -> Neovim r st ()
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
      , Q.col           = Just (col, True)
      , Q.nr            = Nothing
      , Q.text          = ""
      , Q.errorType     = toQFSeverity severity
      }
    f msg = Q.QFItem {
        Q.bufOrFile     = Right ""
      , Q.lnumOrPattern = Right ""
      , Q.col           = Nothing
      , Q.nr            = Nothing
      , Q.text          = msg
      , Q.errorType     = Q.Warning
      }

    toQFSeverity Ghcid.Error   = Q.Error
    toQFSeverity Ghcid.Warning = Q.Warning

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

-- | an alternative to `vim_out_write'` which doesn't work for me :(
nvimEcho :: String -> Neovim r st ()
nvimEcho s = void $ vim_command_output $ "echom " ++ show s

-- | return the canonical path of current working directory

--   `canonicalizePath` is necessary because nvim can return
--   a not canonical path such as "/foo//bar" ('/' duplicated)
nvimCWD :: Neovim r st FilePath
nvimCWD = liftIO . canonicalizePath =<<
  errOnInvalidResult (vim_call_function "getcwd" [])

-- | return the canonical path of the current file's directory
nvimCurrentFileDir :: Neovim r st FilePath
nvimCurrentFileDir = liftIO . canonicalizePath =<<
  errOnInvalidResult (vim_call_function "expand" [ObjectString "%:p:h"])

-- | return the canonical path of the current file
nvimCurrentFile :: Neovim r st FilePath
nvimCurrentFile = liftIO . canonicalizePath =<<
  errOnInvalidResult (vim_call_function "expand" [ObjectString "%:p"])

orThrow :: Pretty e => Neovim r st (Maybe a) -> e -> Neovim r st a
orThrow m e = m >>= \case
  Just x -> return x
  Nothing -> err e

