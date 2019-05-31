{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal
  ( RmmEx(..)
  , mempty
  , size
  , fromWord64s
  , fromPartialWord64s
  , toPartialWord64s
  , fromBools
  ) where

import Data.Coerce
import Data.Monoid
import Data.Word
import HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types (Elem (Elem), RmmEx (RmmEx))
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                                         (ViewL (..), ViewR (..), (|>))
import HaskellWorks.Data.Positioning
import Prelude                                                              hiding (max, min)

import qualified Data.List                                                            as L
import qualified HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types as T
import qualified HaskellWorks.Data.FingerTree                                         as FT

type RmmFt = FT.FingerTree T.Measure T.Elem

empty :: RmmEx
empty = RmmEx FT.empty

size :: RmmEx -> Count
size (RmmEx parens) = T.size ((FT.measure parens) :: T.Measure)

-- TODO Needs optimisation
fromWord64s :: Traversable f => f Word64 -> RmmEx
fromWord64s = foldl go empty
  where go :: RmmEx -> Word64 -> RmmEx
        go rmm w = RmmEx (T.parens rmm |> Elem w 64)

-- TODO Needs optimisation
fromPartialWord64s :: Traversable f => f (Word64, Count) -> RmmEx
fromPartialWord64s = foldl go empty
  where go :: RmmEx -> (Word64, Count) -> RmmEx
        go rmm (w, n) = RmmEx (T.parens rmm |> Elem w n)

toPartialWord64s :: RmmEx -> [(Word64, Count)]
toPartialWord64s = L.unfoldr go . coerce
  where go :: RmmFt -> Maybe ((Word64, Count), RmmFt)
        go ft = case FT.viewl ft of
          T.Elem w n :< rt -> Just ((w, coerce n), rt)
          FT.EmptyL        -> Nothing

fromBools :: [Bool] -> RmmEx
fromBools = go empty
  where go :: RmmEx -> [Bool] -> RmmEx
        go (RmmEx ps) (b:bs) = case FT.viewr ps of
          FT.EmptyR      -> go (RmmEx (FT.singleton (Elem b' 1))) bs
          lt :> Elem w n ->
            let newPs = if n >= 64
                then ps |> Elem b' 1
                else lt |> Elem (w .|. (b' .<. fromIntegral n)) (n + 1)
            in go (RmmEx newPs) bs
          where b' = if b then 1 else 0 :: Word64
        go rmm [] = rmm
