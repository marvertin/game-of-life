module Lib (
  frequency,
  genList
) where

import Data.Char
import Control.Arrow
import Data.List
import Control.Monad

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (length &&& head) (group (sort list))

genList :: (a -> a) -> a -> [a]
genList f x = x : genList f (f x)
