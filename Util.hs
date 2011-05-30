module Util
  ( fAnd 
  , fOr
  , tuplify2
  ) where

import Data.Array.ST

fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)
