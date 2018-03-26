
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Neovim.Ghcid.Simple where

import           Neovim
import           Neovim.Ghcid.Simple.Plugin

import           Data.Map as M (empty)

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
  { exports = []
  , statefulExports =
      [ StatefulFunctionality
          { readOnly = ()
          , writable = GhcidState M.empty 0
          , functionalities =
              [ $(command' 'ghcidCheck)      ["async", "!"]
              , $(command' 'ghcidShowStatus) ["async"]
              , $(command' 'ghcidStopAll)    ["async"]
              ]
          }
      ]
  }

