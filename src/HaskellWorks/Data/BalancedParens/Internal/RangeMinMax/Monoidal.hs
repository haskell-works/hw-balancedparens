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
import Data.Word
import HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types (Elem (Elem), Measure)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                                         (FingerTree, ViewR (..), (|>))
import Prelude                                                              hiding (max, min)

import qualified HaskellWorks.Data.FingerTree as FT

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
        go (RmmEx ps) (b:bs) = case FT.viewr ps of
          FT.EmptyR      -> RmmEx (FT.singleton (Elem b' 1))
          lt :> Elem w n ->
            let newPs = if n >= 64
                then ps |> Elem w 1
                else lt |> Elem (w .|. (1 .<. fromIntegral n)) (n + 1)
            in go (RmmEx newPs) bs
          where b' = if b then 1 else 0 :: Word64
        go rmm [] = rmm

newtype RmmEx = RmmEx
  { parens :: FingerTree Measure Elem
  }

empty :: RmmEx
empty = RmmEx FT.empty
