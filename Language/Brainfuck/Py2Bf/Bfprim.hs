module Language.Brainfuck.Py2Bf.Bfprim
  ( Bfprim (..)
  , isBfprimNonIO
  , isBfWhile
  , BfState
  , runPrim
  , runPrimIO
  ) where

import Data.Word (Word8)
import Data.Functor ((<$>))
import Codec.Binary.UTF8.String (decode, encode)
import Control.Monad.Instances ()

data Bfprim = BfIncr
            | BfDecr
            | BfNext
            | BfPrev
            | BfPut
            | BfGet
            | BfWhile [Bfprim]
            deriving Eq

instance Show Bfprim where
  show BfIncr = "+"
  show BfDecr = "-"
  show BfNext = ">"
  show BfPrev = "<"
  show BfPut  = "."
  show BfGet  = ","
  show (BfWhile bf) = '[' : concatMap show bf ++ "]"
  showList = (++) . concatMap show

instance Read Bfprim where
  readsPrec _ str = case snd (parse str) of
                       bfprim:_ -> [(bfprim, "")]
                       _ -> []
  readList str = [(snd (parse str), "")]

parse :: String -> (String, [Bfprim])
parse ('+':bf) = (BfIncr:) <$> parse bf
parse ('-':bf) = (BfDecr:) <$> parse bf
parse ('>':bf) = (BfNext:) <$> parse bf
parse ('<':bf) = (BfPrev:) <$> parse bf
parse ('.':bf) = (BfPut:) <$> parse bf
parse (',':bf) = (BfGet:) <$> parse bf
parse ('[':bf) = (\(s, bff) -> (BfWhile bff:) <$> parse s) (parse bf)
parse (']':bf) = (bf, [])
parse (_:bf)   = parse bf
parse []       = ("", [])

isBfprimNonIO :: Bfprim -> Bool
isBfprimNonIO BfPut = False
isBfprimNonIO BfGet = False
isBfprimNonIO (BfWhile bf) = all isBfprimNonIO bf
isBfprimNonIO _ = True

isBfWhile :: Bfprim -> Bool
isBfWhile (BfWhile _) = True
isBfWhile _ = False

type BfState = ([Word8], Word8, [Word8], [Word8], [Word8])

runPrim :: [Bfprim] -> BfState -> Either String BfState
runPrim (BfIncr:bf) (xs, x, ys, os, is)   = runPrim bf (xs, x + 1, ys, os, is)
runPrim (BfDecr:bf) (xs, x, ys, os, is)   = runPrim bf (xs, x - 1, ys, os, is)
runPrim (BfNext:bf) (xs, x, [], os, is)   = runPrim bf (x:xs, 0, [], os, is)
runPrim (BfNext:bf) (xs, x, y:ys, os, is) = runPrim bf (x:xs, y, ys, os, is)
runPrim (BfPrev:_) ([], _, _, _, _)       = Left negativeAddressError
runPrim (BfPrev:bf) (x:xs, y, ys, os, is) = runPrim bf (xs, x, y:ys, os, is)
runPrim (BfWhile _:bf) s@(_, 0, _, _, _)  = runPrim bf s
runPrim bf@(BfWhile bff:_) s              = runPrim bff s >>= runPrim bf
runPrim (BfPut:bf) (xs, x, ys, os, is)    = runPrim bf (xs, x, ys, os ++ [x], is)
runPrim (BfGet:bf) (xs, _, ys, os, [])    = runPrim bf (xs, 0, ys, os, [])
runPrim (BfGet:bf) (xs, _, ys, os, i:is)  = runPrim bf (xs, i, ys, os, is)
runPrim [] s                              = Right s

runPrimIO :: [Bfprim] -> BfState -> IO BfState
runPrimIO (BfIncr:bf) (xs, x, ys, os, is)   = runPrimIO bf (xs, x + 1, ys, os, is)
runPrimIO (BfDecr:bf) (xs, x, ys, os, is)   = runPrimIO bf (xs, x - 1, ys, os, is)
runPrimIO (BfNext:bf) (xs, x, [], os, is)   = runPrimIO bf (x:xs, 0, [], os, is)
runPrimIO (BfNext:bf) (xs, x, y:ys, os, is) = runPrimIO bf (x:xs, y, ys, os, is)
runPrimIO (BfPrev:_) ([], _, _, _, _)       = error negativeAddressError
runPrimIO (BfPrev:bf) (x:xs, y, ys, os, is) = runPrimIO bf (xs, x, y:ys, os, is)
runPrimIO (BfWhile _:bf) s@(_, 0, _, _, _)  = runPrimIO bf s
runPrimIO bf@(BfWhile bff:_) s              = runPrimIO bff s >>= runPrimIO bf
runPrimIO (BfPut:bf) (xs, x, ys, os, is)
  | x > 0xbf = putStr (decode os) >> runPrimIO bf (xs, x, ys, [x], is)
  | null os || l == 5 && 0xfb < h && h <= 0xff
            || l == 4 && 0xf7 < h && h <= 0xfb
            || l == 3 && 0xef < h && h <= 0xf7
            || l == 2 && 0xdf < h && h <= 0xef
            || l == 1 && 0xc1 < h && h <= 0xdf
              = putStr str >> runPrimIO bf (xs, x, ys, [], is)
  | otherwise = runPrimIO bf (xs, x, ys, os ++ [x], is)
  where str = decode (os ++ [x])
        l = length os
        h = head os
runPrimIO (BfGet:bf) (xs, _, ys, os, [])    = encode . return <$> getChar >>=
                         \inp -> case inp of
                                      []   -> runPrimIO bf (xs, 0, ys, os, [])
                                      i:is -> runPrimIO bf (xs, i, ys, os, is)
runPrimIO (BfGet:bf) (xs, _, ys, os, i:is)  = runPrimIO bf (xs, i, ys, os, is)
runPrimIO [] s                              = return s

negativeAddressError :: String
negativeAddressError = "negative address access"
