module Language.Brainfuck.Py2Bf.Bf
  ( Bf (..)
  , Bfcode (..)
  , Bfpointer
  ) where

import Data.List (intercalate)
import Language.Brainfuck.Py2Bf.Bfprim

class Bf a where
  convert :: a -> Bfcode
  upconvert :: Bfcode -> a

data Bfcode = Bfcode [Bfprim]
            deriving Eq

instance Show Bfcode where
  show (Bfcode bf) = intercalate "\n" (spl 80 (show (compress bf)))

instance Read Bfcode where
  readsPrec _ str = [(Bfcode (read str :: [Bfprim]), "")]

type Bfpointer = Integer

spl :: Int -> [a] -> [[a]]
spl d xs | null rs   = [r]
         | otherwise = r : spl d rs
  where (r, rs) = splitAt d xs

compress :: [Bfprim] -> [Bfprim]
compress = trailingNonIO . succWhile . headingWhile

succWhile :: [Bfprim] -> [Bfprim]
succWhile (BfWhile bff : bf)
  = BfWhile (succWhile bff) : succWhile (dropWhile isBfWhile bf)
succWhile (b : bf) = b : succWhile bf
succWhile [] = []

headingWhile :: [Bfprim] -> [Bfprim]
headingWhile = dropWhile isBfWhile

trailingNonIO :: [Bfprim] -> [Bfprim]
trailingNonIO = reverse . dropWhile isBfprimNonIO . reverse
