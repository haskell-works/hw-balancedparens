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
  , toBools
  , drop
  , firstChild
  ) where

import Data.Coerce
import Data.Foldable
import Data.Monoid
import Data.Word
import HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types (Elem (Elem), Measure, RmmEx (RmmEx))
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                                         (ViewL (..), ViewR (..), (<|), (|>))
import HaskellWorks.Data.Positioning
import Prelude                                                              hiding (drop, max, min)

import qualified Data.List                                                            as L
import qualified HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types as T
import qualified HaskellWorks.Data.BalancedParens.Internal.Word                       as W
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

toBools :: RmmEx -> [Bool]
toBools rmm = toBoolsDiff rmm []

toBoolsDiff :: RmmEx -> [Bool] -> [Bool]
toBoolsDiff rmm = mconcat (fmap go (toPartialWord64s rmm))
  where go :: (Word64, Count) -> [Bool] -> [Bool]
        go (w, n) = W.partialToBoolsDiff (fromIntegral n) w

drop :: Count -> RmmEx -> RmmEx
drop n (RmmEx parens) = case FT.split predicate parens of
  (lt, rt) -> let n' = n - T.size (FT.measure lt :: T.Measure) in
    case FT.viewl rt of
      T.Elem w nw :< rrt -> if n' >= nw
        then RmmEx rrt
        else RmmEx ((T.Elem (w .>. n') (nw - n')) <| rrt)
      FT.EmptyL          -> empty
  where predicate :: Measure -> Bool
        predicate m = n < T.size (m :: Measure)

firstChild  :: RmmEx -> Count -> Maybe Count
firstChild rmm n = case FT.viewl ft of
  T.Elem w nw :< rt -> if nw >= 2
    then case w .&. 3 of
      3 -> Just (n + 1)
      _ -> Nothing
    else if nw >= 1
      then case w .&. 1 of
        1 -> case FT.viewl rt of
          T.Elem w' nw' :< _ -> if nw' >= 1
            then case w' .&. 1 of
              1 -> Just (n + 1)
              _ -> Nothing
            else Nothing
          FT.EmptyL -> Nothing
        _ -> Nothing
      else Nothing
  FT.EmptyL -> Nothing
  where RmmEx ft = drop (n - 1) rmm
