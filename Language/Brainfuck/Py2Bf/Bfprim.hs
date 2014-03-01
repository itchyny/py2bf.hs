module Language.Brainfuck.Py2Bf.Bfprim
  ( Bfprim (..)
  , isBfprimNonIO
  , isBfWhile
  ) where

import Data.Functor ((<$>))

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
