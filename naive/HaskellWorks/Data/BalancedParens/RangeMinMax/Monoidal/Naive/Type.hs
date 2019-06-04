{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.RangeMinMax.Monoidal.Naive.Type
  ( RmmEx(..)
  , Elem(..)
  , Size(..)
  ) where

import Data.Semigroup               (Semigroup ((<>)))
import HaskellWorks.Data.FingerTree (FingerTree)

import qualified HaskellWorks.Data.FingerTree as FT

newtype Elem a  = Elem { getElem :: a } deriving (Eq, Show)

newtype Size = Size { getSize :: Int } deriving (Eq, Ord)

instance Semigroup Size where
  Size a <> Size b = Size (a + b)

instance Monoid Size where
  mempty = Size 0

instance FT.Measured Size (Elem Bool) where
  measure _ = Size 1

newtype RmmEx = RmmEx
  { parens :: FingerTree Size (Elem Bool)
  }
