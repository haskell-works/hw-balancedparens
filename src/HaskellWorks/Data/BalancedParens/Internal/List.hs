module HaskellWorks.Data.BalancedParens.Internal.List
  ( chunkBy
  ) where

chunkBy :: Int -> [a] -> [[a]]
chunkBy n bs = case (take n bs, drop n bs) of
  (as, zs) -> if null zs then [as] else as:chunkBy n zs
