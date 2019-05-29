module HaskellWorks.Data.BalancedParens.Internal.List
  ( chunkBy
  , toBools
  ) where

import Data.Word

import qualified HaskellWorks.Data.BalancedParens.Internal.Word as W

chunkBy :: Int -> [a] -> [[a]]
chunkBy n bs = case (take n bs, drop n bs) of
  (as, zs) -> if null zs then [as] else as:chunkBy n zs

toBoolsDiff :: [Word64] -> [Bool] -> [Bool]
toBoolsDiff (w:ws) = W.toBoolsDiff w . toBoolsDiff ws
toBoolsDiff []     = id

toBools :: [Word64] -> [Bool]
toBools ws = toBoolsDiff ws []
