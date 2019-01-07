
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Neovim.Ghcid.Simple.Plugin where

import           RIO
import qualified RIO.List                               as L
import qualified RIO.Map                                as M
import qualified RIO.Set                                as S

import           Control.Lens                           (makeLenses, (%~), (<&>))
import           Control.Monad.Extra                    (whenJust)
import           Distribution.PackageDescription        (GenericPackageDescription (..))
import           Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import           Distribution.Types.UnqualComponentName (unUnqualComponentName)
import           Distribution.Verbosity                 (silent)
import qualified Language.Haskell.Ghcid                 as Ghcid

import           Neovim
import           Neovim.BuildTool
import qualified Neovim.Quickfix                        as Q
import           Neovim.User.Choice                     (askForIndex)
import           System.Directory                       (canonicalizePath,
                                                         getDirectoryContents)
import           System.FilePath                        (dropExtension,
                                                         isRelative, (</>))
import           System.IO.Unsafe                       (unsafePerformIO)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Target = Target
  { _targetID        :: !Int
  , _targetName      :: !String
  , _targetCabalFile :: !FilePath
  , _targetGhci      :: !Ghcid.Ghci
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

{-# NOINLINE initialEnv #-}
initialEnv :: GhcidEnv
initialEnv = GhcidEnv
  { targets = unsafePerformIO (newTVarIO M.empty)
  , unique  = unsafePerformIO (newTVarIO 0)
  }

genUnique :: NeovimGhcid Int
genUnique = do
  v <- asks unique
  atomically $ do
    modifyTVar' v succ
    readTVar v

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

ghcidCheck :: CommandArguments -> Maybe String -> NeovimGhcid ()
ghcidCheck ca mFile = do
    file <- maybe nvimCurrentFile return mFile
    mtgt <- lookupTargetByFile file >>= \case
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
    else nvimEcho $ init $ unlines $ concat
            [ t^.targetName : map ("  "++) (S.toList (moduleNamesInTarget t) <&> id)
            | (_,t) <- M.toList tgts ]

ghcidStopAll :: CommandArguments -> NeovimGhcid ()
ghcidStopAll _ = do
  tgts <- atomically . (`swapTVar` M.empty) =<< asks targets
  mapM_ stopTarget tgts

ghcidExec :: CommandArguments -> String -> NeovimGhcid ()
ghcidExec _ = ghcidExecute (Just "/tmp/ghcid-nvim-simple.preview")

ghcidType :: CommandArguments -> String -> NeovimGhcid ()
ghcidType _ expr = ghcidExecute Nothing $ ":type " ++ expr

ghcidTypeCurrentWord :: CommandArguments -> NeovimGhcid ()
ghcidTypeCurrentWord ca = do
    word <- nvimCword
    ghcidType ca word

ghcidExecute :: Maybe FilePath -> String -> NeovimGhcid ()
ghcidExecute mbuffer cmd = do
    file <- nvimCurrentFile
    lookupTargetByFile file >>= \case
      Nothing -> vim_report_error' "current file is not loaded"
      Just tgt -> do
        result <- execCmdTarget tgt cmd
        let content = init $ unlines result
        case mbuffer of
          Nothing -> nvimEchoe content
          Just f -> do
            writeFileUtf8Builder f (fromString content)
            vim_command' $ "pedit " ++ f

-- TODO TypeCurrentWord, TypeVisual

-------------------------------------------------------------------------------
-- Wrapper
-------------------------------------------------------------------------------

-- Target (wrapper of Ghci)
---------------------------

data TargetConfig = TargetConfig
  { cProjectRoot :: Directory
  , cCabalFile   :: FilePath
  , cTargetName  :: String
  , cCommandStr  :: String
  }

newTarget :: TargetConfig -> NeovimGhcid (Target, [QfItem])
newTarget TargetConfig{..} = do
    (ghci, loads) <-
        liftIO
          (Ghcid.startGhci
            cCommandStr
            (Just projectRootDir)
            (\_ _ -> return ()))
      `withException`
        (vim_report_error' . show @SomeException)
    tgt <- do
      files <- liftIO $
        S.fromList . map (second mkAbosolute) <$> Ghcid.showModules ghci
      id'   <- genUnique
      return $ Target id' cTargetName cCommandStr ghci files
    addTarget tgt
    return (tgt, loadsToQFItems loads)
  where
    projectRootDir = getDirectory cProjectRoot
    mkAbosolute f
      | isRelative f = projectRootDir </> f
      | otherwise    = f

addTarget :: Target -> NeovimGhcid ()
addTarget p = targets %== M.insert (p^.targetID) p

reloadTarget :: Target -> NeovimGhcid Target
reloadTarget tgt@Target{_targetGhci=g} = do
    setQfList . loadsToQFItems =<< liftIO (Ghcid.reload g)
    files <- S.fromList <$> liftIO (Ghcid.showModules g)
    let tgt' = tgt & targetFiles %~ S.union files
    addTarget tgt'
    return tgt'

stopTarget :: Target -> NeovimGhcid ()
stopTarget t = liftIO $ Ghcid.stopGhci (t^.targetGhci)

execCmdTarget :: Target -> String -> NeovimGhcid [String]
execCmdTarget tgt cmd = liftIO $ Ghcid.exec (tgt^.targetGhci) cmd

lookupTargetByFile :: FilePath -> NeovimGhcid (Maybe Target)
lookupTargetByFile f = uses' targets $ L.find (`hasFile` f)

lookupTargetByTgtName :: String -> NeovimGhcid (Maybe Target)
lookupTargetByTgtName tgtName = uses' targets $ L.find ((tgtName==) . (^.targetName))

hasFile :: Target -> FilePath -> Bool
hasFile t f = f `S.member` filesInTarget t

-- QuickfixListItem
-------------------

type QfItem = Q.QuickfixListItem String

-- | Convert 'Ghcid.Load' to Quickfix Items
loadsToQFItems :: [Ghcid.Load] -> [QfItem]
loadsToQFItems = concatMap loadToQFItems . filterLoads
  where
    loadToQFItems :: Ghcid.Load -> [QfItem]
    loadToQFItems (Ghcid.Message severity file (lnum,col) _posEnd msgs) =
        header : map f msgs
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
    loadToQFItems Ghcid.Loading{} = []
    loadToQFItems Ghcid.LoadConfig{} = []

    filterLoads :: [Ghcid.Load] -> [Ghcid.Load]
    filterLoads loads = loads'
      where
        loads' = case filter isError loads of
                   [] -> loads
                   ls -> ls
        isError l = isMessage l && Ghcid.loadSeverity l==Ghcid.Error
        isMessage Ghcid.Message{}    = True
        isMessage Ghcid.Loading{}    = False
        isMessage Ghcid.LoadConfig{} = False

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

newGhci :: Bool -> NeovimGhcid (Maybe Target)
newGhci default' = do
  dir <- Directory <$> nvimCurrentFileDir
  guessProjectSettings (thisAndParentDirectories dir) >>= askTarget default' >>= \case
    Just config ->
      lookupTargetByTgtName (cTargetName config) >>= \case
        Nothing -> do
          (tgt, loads) <- newTarget config
          setQfList loads
          return $ Just tgt
        Just tgt -> Just <$> reloadTarget tgt
    Nothing -> vim_report_error' "canceled" >> return Nothing

askTarget
  :: Bool
  -> Maybe (BuildTool, Directory)
  -> NeovimGhcid (Maybe TargetConfig)
askTarget default' setting = do
    (baseCmd, dir@(Directory dirPath)) <- case setting of
        (Just (Stack, dir))   -> return ("stack repl", dir)
        (Just (Cabal{}, dir)) -> return ("cabal repl", dir)
        _ -> let msg = "*.cabal not found"
              in vim_report_error' msg >> err (pretty msg)
    cabal <- getCabalFile dirPath
    let cabalFile   = dirPath </> cabal
        packageName = dropExtension cabal
        defaultCfg  = TargetConfig
          { cProjectRoot = dir
          , cCabalFile   = cabalFile
          , cTargetName  = packageName++"-default"
          , cCommandStr  = baseCmd
          }
    if default'
      then return (Just defaultCfg)
      else do
        candidates <- liftIO $ do
          description <- readGenericPackageDescription silent cabalFile
          return $ enumCandidates description packageName
        answer <- askForIndex $
          map (ObjectString . fromString @ByteString)
            [ "("++show n++") " ++ baseCmd ++ " " ++ c
            |  (n,c) <- zip [1::Int ..] ("" : candidates) ]
        case answer of
          Just 1 -> return $ Just defaultCfg
          Just n -> return $ Just defaultCfg
                      { cTargetName = tgtName
                      , cCommandStr = baseCmd ++ " " ++ tgtName
                      }
              where tgtName = candidates!!(n-2)
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
  case L.find (".cabal" `L.isSuffixOf`) files of
    Just f  -> return f
    Nothing -> err "impossible: *.cabal not found"

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

-- Neovim
---------

-- | an alternative to `vim_out_write'` which doesn't work for me :(
nvimEcho :: String -> Neovim env ()
nvimEcho s = vim_command' $ "echo " ++ show s

nvimEchom :: String -> Neovim env ()
nvimEchom s = vim_command' $ "echomsg " ++ show s

nvimEchoe :: String -> Neovim env ()
nvimEchoe s =
    vim_command' $ L.intercalate "|"
      [ "echohl ErrorMsg"
      , "echomsg " ++ show s
      , "echohl None"
      ]

nvimEchow :: String -> Neovim env ()
nvimEchow s =
    vim_command' $ L.intercalate "|"
      [ "echohl WarningMsg"
      , "echomsg " ++ show s
      , "echohl None"
      ]



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

nvimCword :: Neovim env String
nvimCword = errOnInvalidResult $ vim_call_function "expand" [ObjectString "<cword>"]

setQfList :: [QfItem] -> Neovim r ()
setQfList qfs = do
  Q.setqflist qfs Q.Replace
  if null qfs
    then void $ vim_command "echo \"ghcid-nvim: no error found\""
    else void $ vim_command "botright copen"

-- TVar
-------

use' :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> m a
use' getter = readTVarIO =<< asks getter

uses' :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> (a -> r) -> m r
uses' getter f = f <$> use' getter

(.==) :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> a -> m ()
getter .== x = do
  v <- asks getter
  atomically $ writeTVar v x
(%==) :: (MonadReader s m, MonadIO m) => (s -> TVar a) -> (a -> a) -> m ()
getter %== f = do
  v <- asks getter
  atomically $ modifyTVar' v f

