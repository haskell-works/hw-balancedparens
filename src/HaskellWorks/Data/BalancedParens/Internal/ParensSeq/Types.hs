{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module HaskellWorks.Data.BalancedParens.Internal.ParensSeq.Types
  ( Elem(..)
  , Measure(..)
  , ParensSeq(..)
  ) where

import Data.Int
import Data.Monoid
import Data.Semigroup                                (Semigroup ((<>)))
import Data.Word
import HaskellWorks.Data.Excess.PartialMinMaxExcess1
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Positioning
import Prelude                                       hiding (max, min)

import qualified HaskellWorks.Data.FingerTree as FT
import qualified Prelude                      as P

data Elem = Elem
  { bps  :: {-# UNPACK #-} !Word64
  , size :: {-# UNPACK #-} !Count
  } deriving (Eq, Show)

data Measure = Measure
  { size   :: {-# UNPACK #-} !Count
  , min    :: {-# UNPACK #-} !Int
  , excess :: {-# UNPACK #-} !Int
  , max    :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord, Show)

newtype ParensSeq = ParensSeq
  { parens :: FT.FingerTree Measure Elem
  } deriving Show

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
  measure (Elem w size) = Measure { min, excess, max, size }
    where Triplet min excess max = partialMinMaxExcess1 (fromIntegral size) w
