module HaskellWorks.Data.BalancedParens.Internal.List
  ( chunkBy
  , toBools
  , toBalancedParensString
  ) where

import Data.Word

import qualified HaskellWorks.Data.BalancedParens.Internal.Word as W

chunkBy :: Int -> [a] -> [[a]]
chunkBy n bs = case splitAt n bs of
  (as, zs) -> if null zs then [as] else as:chunkBy n zs

toBoolsDiff :: [Word64] -> [Bool] -> [Bool]
toBoolsDiff = foldr ((.) . W.toBoolsDiff) id

toBools :: [Word64] -> [Bool]
toBools ws = toBoolsDiff ws []

toBalancedParensString :: [Bool] -> String
toBalancedParensString (True:bs)  = '(':toBalancedParensString bs
toBalancedParensString (False:bs) = ')':toBalancedParensString bs
toBalancedParensString []         = ""
