{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal
  ( RmmEx(..)
  , fromBools
  , mempty
  , fromWord64s
  , fromWord64s'
  , firstChild
  , nextSibling
  ) where

import Data.Monoid
import Data.Word
import HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types (Elem (Elem), Measure, RmmEx (RmmEx))
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                                         (ViewL (..), ViewR (..), (<|), (|>))
import HaskellWorks.Data.Positioning
import Prelude                                                              hiding (max, min)

import qualified HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal.Types as T
import qualified HaskellWorks.Data.FingerTree                                         as FT

-- TODO Needs optimisation
fromWord64s :: Traversable f => f Word64 -> RmmEx
fromWord64s = foldl go empty
  where go :: RmmEx -> Word64 -> RmmEx
        go rmm w = RmmEx (T.parens rmm |> Elem w 64)

-- TODO Needs optimisation
fromWord64s' :: Traversable f => f (Word64, Count) -> RmmEx
fromWord64s' = foldl go empty
  where go :: RmmEx -> (Word64, Count) -> RmmEx
        go rmm (w, n) = RmmEx (T.parens rmm |> Elem w n)

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

dropRmmEx :: Count -> RmmEx -> RmmEx
dropRmmEx n (RmmEx parens) = case FT.split predicate parens of
  (lt, rt) -> let n' = n - T.size (FT.measure lt :: T.Measure) in
    case FT.viewl rt of
      T.Elem w nw :< rrt -> if n >= nw
        then RmmEx rrt
        else RmmEx ((T.Elem (w .>. n') (n - n')) <| rt)
      FT.EmptyL          -> empty
  where predicate :: Measure -> Bool
        predicate m = n < T.size (m :: Measure)

firstChild  :: RmmEx -> Count -> Maybe Count
firstChild rmm n = case FT.viewl ft of
          T.Elem w nw :< rrt -> if nw >= 2
            then if w .|. 3 == 3
              then Just (n + 1)
              else if nw == 1
                then case FT.viewl rrt of
                  T.Elem w' nw' :< _ -> if nw' >= 1 && w' .|. 1 == 1
                    then Just (n + 1)
                    else error "Empty Elem"
                  FT.EmptyL -> Nothing
                else error "Empty Elem"
            else error "Empty Elem"
          FT.EmptyL -> Nothing
  where RmmEx ft = dropRmmEx n rmm

nextSibling  :: RmmEx -> Count -> Maybe Count
nextSibling rmm n = case FT.split predicate ft of
  (lt, rt) -> let n' = n - T.size (FT.measure lt :: T.Measure) in
    case FT.viewl rt of
      T.Elem w nw :< rrt -> if n >= nw
        -- TODO The argumen to pick is wrong because lt doesn't have all the bits.
        then pick (T.min (FT.measure lt :: T.Measure)) rrt
        else pick (T.min (FT.measure lt :: T.Measure)) (T.Elem (w .>. n') (n - n') <| rt)
      FT.EmptyL -> pick (T.min (FT.measure lt :: T.Measure)) FT.empty
  where predicate :: Measure -> Bool
        predicate m = T.min (m :: Measure) < 0
        RmmEx ft = dropRmmEx n rmm
        pick :: Int -> FT.FingerTree Measure Elem -> Maybe Count
        pick mn _ = Nothing

-- parent      :: RmmEx -> Count -> Maybe Count

empty :: RmmEx
empty = RmmEx FT.empty
