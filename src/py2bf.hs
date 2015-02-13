module Main where

import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Paths_py2bf (version)
import System.Console.GetOpt
import System.Environment (getArgs)

data Flag = Input String
          | Output String
          | Version
          | Help
          deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option "i"  ["input"]   (OptArg (Input  . fromMaybe "stdin" ) "FILE") "input FILE"
  , Option "o"  ["output"]  (OptArg (Output . fromMaybe "stdout") "FILE") "output FILE"
  , Option "vV" ["version"] (NoArg Version) "show the version number"
  , Option "h?" ["help"]    (NoArg Help)    "display this help"
  ]

command :: ([Flag], [String], [String]) -> IO ()
command (opt, nonopt, err)
  | Help `elem` opt || null opt && null err = putStr $ usageInfo (info ++ usage) options
  | Version `elem` opt = putStr info
  | null err = print (opt, nonopt, err)
  | otherwise = ioError (userError (concat err ++ usageInfo usage options))
  where info = "py2bf: version " ++ showVersion version ++ "\n"
        usage = "usage: [OPTION...] file"

main :: IO ()
main = command . getOpt Permute options =<< getArgs
