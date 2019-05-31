module HaskellWorks.Data.BalancedParens.Internal.RoseTree
  ( RoseTree(..)
  , toBools
  , toBools'
  , size
  , depth
  ) where

newtype RoseTree = RoseTree
  { children :: [RoseTree]
  } deriving (Eq, Show)

toBools :: RoseTree -> [Bool]
toBools rt = toBools' rt []

toBools' :: RoseTree -> [Bool] -> [Bool]
toBools' (RoseTree cs) = (True:) . foldr (.) id (fmap toBools' cs) . (False:)

size :: RoseTree -> Int
size (RoseTree cs) = 1 + sum (fmap size cs)

depth :: RoseTree -> Int
depth (RoseTree cs) = 1 + maximum (fmap depth cs)
