{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal
  ( RmmEx(..)
  , fromBools
  , mempty
  , fromWord64s
  , fromWord64s'
  ) where

import Data.Int
import Data.Monoid
import Data.Semigroup                 (Semigroup ((<>)))
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree   (FingerTree, ViewR (..), (<|), (|>))
import Prelude                        hiding (max, min)

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

-- TODO Needs optimisation
fromWord64s :: Traversable f => f Word64 -> RmmEx
fromWord64s = foldl go empty
  where go :: RmmEx -> Word64 -> RmmEx
        go rmm w = RmmEx (parens rmm |> Elem w 64)

-- TODO Needs optimisation
fromWord64s' :: Traversable f => f (Word64, Int) -> RmmEx
fromWord64s' = foldl go empty
  where go :: RmmEx -> (Word64, Int) -> RmmEx
        go rmm (w, n) = RmmEx (parens rmm |> Elem w n)

fromBools :: [Bool] -> RmmEx
fromBools = go empty
  where go :: RmmEx -> [Bool] -> RmmEx
        go (RmmEx parens) (b:bs) = case FT.viewr parens of
          FT.EmptyR      -> RmmEx (FT.singleton (Elem b' 1))
          lt :> Elem w n -> if n >= 64
            then RmmEx (parens |> Elem w 1)
            else RmmEx (lt |> Elem (w .|. (1 .<. fromIntegral n)) (n + 1))
          where b' = if b then 1 else 0 :: Word64
-- fromBools cs = RmmEx (foldl go FT.empty cs)
--   where go :: FingerTree Measure Elem -> Bool -> FingerTree Measure Elem
--         go _ _ = error "implement fromBools"

newtype RmmEx = RmmEx
  { parens :: FingerTree Measure Elem
  }

empty :: RmmEx
empty = RmmEx FT.empty
