{-# LANGUAGE QuasiQuotes #-}
module Main (
  main
) where

import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_ire (getDataDir, version) -- from cabal
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc)
import qualified System.Console.Docopt.NoTH as O

import IRE.Server (server)

usage :: IO String
usage = do
  dataDir <- getDataDir
  return $
    "IRE " ++ showVersion version ++
    " - Watching you!" ++ [qc|

Usage:
  ire [options]

Options:
  -c, --config=FILE        Configuration file [default: ire.yml]

  -r, --rootdir=DIR        Web root directory with static files [default: {dataDir}]
  -h, --help               Show this message

Note:
  The default configuration file is loaded if found,
  otherwise default built-in settings are used.

|]

main :: IO ()
main = do
  doco <- O.parseUsageOrExit =<< usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let configFile = fromJust . O.getArg args $ O.longOption "config"
        rootDir  = fromJust . O.getArg args $ O.longOption "rootdir"
    server rootDir configFile

