module HaskellWorks.Data.BalancedParens.Internal.RoseTree
  ( RoseTree(..)
  , toBools
  ) where

data RoseTree = RoseTree
  { children :: [RoseTree]
  } deriving (Eq, Show)

toBools :: RoseTree -> [Bool]
toBools rt = toBools' rt []

toBools' :: RoseTree -> [Bool] -> [Bool]
toBools' (RoseTree cs) = (True:) . foldMap toBools' cs . (False:)
