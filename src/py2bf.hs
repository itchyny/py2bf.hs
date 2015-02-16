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

isInput :: Flag -> Bool
isInput (Input _) = True
isInput _ = False

isOutput :: Flag -> Bool
isOutput (Output _) = True
isOutput _ = False

arg :: Flag -> String
arg (Input str) = str
arg (Output str) = str
arg _ = ""

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
  | null err && null inputfiles = putStrLn "py2bf: no input files"
  | null err && length inputfiles > 1 = putStrLn "py2bf: too many input files"
  | null err = process (last inputfiles) (last ("" : map arg (filter isOutput opt)))
  | otherwise = ioError (userError (concat err ++ usageInfo usage options))
  where info = "py2bf: version " ++ showVersion version ++ "\n"
        usage = "usage: [OPTION...] file"
        inputfiles = map arg (filter isInput opt) ++ nonopt

process :: String -> String -> IO ()
process input output = print (input, output)

main :: IO ()
main = command . getOpt Permute options =<< getArgs
