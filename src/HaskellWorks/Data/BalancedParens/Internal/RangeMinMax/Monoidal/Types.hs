{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types
  ( Elem(..)
  , Measure(..)
  ) where

import Data.Int
import Data.Monoid
import Data.Semigroup (Semigroup ((<>)))
import Data.Word
import Prelude        hiding (max, min)

import qualified HaskellWorks.Data.FingerTree as FT
import qualified Prelude                      as P

data Elem = Elem
  { bps  :: {-# UNPACK #-} !Word64
  , size :: {-# UNPACK #-} !Int
  } deriving (Eq, Show)

data Measure = Measure
  { size   :: Int
  , min    :: Int
  , max    :: Int
  , excess :: Int
  } deriving (Eq, Ord)

instance Semigroup Measure where
  Measure aSize aMin aMax aExcess <> Measure bSize bMin bMax bExcess = Measure
    { size    = aSize + bSize
    , min     = P.min aMin (bMin + aExcess)
    , max     = P.max aMax (bMax + aExcess)
    , excess  = aExcess + bExcess
    }

instance Monoid Measure where
  mempty = Measure 0 0 0 0

instance FT.Measured Measure Elem where
  measure e = Measure (size (e :: Elem)) undefined undefined undefined
