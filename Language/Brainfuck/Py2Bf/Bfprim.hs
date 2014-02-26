module Language.Brainfuck.Py2Bf.Bfprim
  ( Bfprim (..)
  , isBfprimIO
  , isBfWhile
  ) where

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
parse ('+':bf) = fm (BfIncr:) (parse bf)
parse ('-':bf) = fm (BfDecr:) (parse bf)
parse ('>':bf) = fm (BfNext:) (parse bf)
parse ('<':bf) = fm (BfPrev:) (parse bf)
parse ('.':bf) = fm (BfPut:) (parse bf)
parse (',':bf) = fm (BfGet:) (parse bf)
parse ('[':bf) = (\(s, bff) -> fm (BfWhile bff:) (parse s)) (parse bf)
parse (']':bf) = (bf, [])
parse (_:bf)   = parse bf
parse []       = ("", [])

fm :: (b -> c) -> (a, b) -> (a, c)
fm f (a, b) = (a, f b)

isBfprimIO :: Bfprim -> Bool
isBfprimIO BfPut = True
isBfprimIO BfGet = True
isBfprimIO (BfWhile bf) = any isBfprimIO bf
isBfprimIO _ = False

isBfWhile :: Bfprim -> Bool
isBfWhile (BfWhile _) = True
isBfWhile _ = False
