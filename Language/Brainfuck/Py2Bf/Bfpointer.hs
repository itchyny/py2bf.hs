module Language.Brainfuck.Py2Bf.Bfpointer
  ( Bfp (..)
  , Bfpcode (..)
  )
  where

import Data.Functor ((<$>))
import Control.Monad.Instances ()
import Language.Brainfuck.Py2Bf.Bf
import Language.Brainfuck.Py2Bf.Bfprim

data Bfpcode = Bfpcode [Bfp]
             deriving (Eq, Show)

data Bfp = Addp Bfpointer Int
         | Putp Bfpointer
         | Getp Bfpointer
         | Whilep Bfpointer [Bfp]
         | Rawp Bfpointer Bfpointer [Bfprim]
         deriving (Eq, Show)

instance Bf Bfpcode where
  convert (Bfpcode bf) = Bfcode (snd (cvt (compress bf) 0))

type Bfpstate = Bfpointer

cvt :: [Bfp] -> Bfpstate -> (Bfpstate, [Bfprim])
cvt (Addp p i : bf) s
  | i > 0     = (move s p++) <$> (replicate i BfIncr++) <$> cvt bf p
  | i < 0     = (move s p++) <$> (replicate (-i) BfDecr++) <$> cvt bf p
  | otherwise = cvt bf s
cvt (Putp p : bf) s = (move s p++) <$> (BfPut:) <$> cvt bf p
cvt (Getp p : bf) s = (move s p++) <$> (BfGet:) <$> cvt bf p
cvt (Whilep p bff : bf) s
  = let (q, bfff) = cvt bff p
        in (move s p++) <$> (BfWhile (bfff ++ move q p):) <$> cvt bf p
cvt (Rawp p q bff : bf) s = (move s p++) <$> (bff++) <$> cvt bf q
cvt [] s = (s, [])

move :: Bfpstate -> Bfpstate -> [Bfprim]
move p q | p < q     = replicate (fromEnum (q - p)) BfNext
         | otherwise = replicate (fromEnum (p - q)) BfPrev

compress :: [Bfp] -> [Bfp]
compress = headingWhile

headingWhile :: [Bfp] -> [Bfp]
headingWhile = dropWhile isBfpWhile

isBfpWhile :: Bfp -> Bool
isBfpWhile (Whilep _ _) = True
isBfpWhile _ = False
