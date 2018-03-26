
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Neovim.Ghcid.Simple where

import           Neovim
import           Neovim.Ghcid.Simple.Plugin

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = wrapPlugin Plugin
  { environment = initialEnv
  , exports = [ $(command' 'ghcidCheck)      ["async", "!"]
              , $(command' 'ghcidShowStatus) ["async"]
              , $(command' 'ghcidStopAll)    ["async"]
              ]
  }

