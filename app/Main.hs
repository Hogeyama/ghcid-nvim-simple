module Main where

import Neovim
import Neovim.Ghcid.Simple (plugin)

main :: IO ()
main = neovim defaultConfig { plugins = [ plugin ] }
