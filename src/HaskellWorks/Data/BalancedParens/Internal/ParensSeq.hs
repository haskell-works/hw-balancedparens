{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.Internal.ParensSeq
  ( ParensSeq(..)
  , mempty
  , size
  , fromWord64s
  , fromPartialWord64s
  , toPartialWord64s
  , fromBools
  , toBools
  , drop
  , drop2
  , firstChild
  , nextSibling
  , (<|), (><), (|>)
  ) where

import Data.Coerce
import Data.Foldable
import Data.Monoid
import Data.Word
import HaskellWorks.Data.BalancedParens.Internal.ParensSeq.Internal (Elem (Elem), ParensSeq (ParensSeq), ParensSeqFt)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                                 (ViewL (..), ViewR (..), (<|), (><), (|>))
import HaskellWorks.Data.Positioning
import Prelude                                                      hiding (drop, max, min)

import qualified Data.List                                                    as L
import qualified HaskellWorks.Data.BalancedParens.Internal.ParensSeq.Internal as T
import qualified HaskellWorks.Data.BalancedParens.Internal.Word               as W
import qualified HaskellWorks.Data.FingerTree                                 as FT

empty :: ParensSeq
empty = ParensSeq FT.empty

size :: ParensSeq -> Count
size (ParensSeq parens) = T.size ((FT.measure parens) :: T.Measure)

-- TODO Needs optimisation
fromWord64s :: Traversable f => f Word64 -> ParensSeq
fromWord64s = foldl go empty
  where go :: ParensSeq -> Word64 -> ParensSeq
        go rmm w = ParensSeq (T.parens rmm |> Elem w 64)

-- TODO Needs optimisation
fromPartialWord64s :: Traversable f => f (Word64, Count) -> ParensSeq
fromPartialWord64s = foldl go empty
  where go :: ParensSeq -> (Word64, Count) -> ParensSeq
        go rmm (w, n) = ParensSeq (T.parens rmm |> Elem w n)

toPartialWord64s :: ParensSeq -> [(Word64, Count)]
toPartialWord64s = L.unfoldr go . coerce
  where go :: ParensSeqFt -> Maybe ((Word64, Count), ParensSeqFt)
        go ft = case FT.viewl ft of
          T.Elem w n :< rt -> Just ((w, coerce n), rt)
          FT.EmptyL        -> Nothing

fromBools :: [Bool] -> ParensSeq
fromBools = go empty
  where go :: ParensSeq -> [Bool] -> ParensSeq
        go (ParensSeq ps) (b:bs) = case FT.viewr ps of
          FT.EmptyR      -> go (ParensSeq (FT.singleton (Elem b' 1))) bs
          lt :> Elem w n ->
            let newPs = if n >= 64
                then ps |> Elem b' 1
                else lt |> Elem (w .|. (b' .<. fromIntegral n)) (n + 1)
            in go (ParensSeq newPs) bs
          where b' = if b then 1 else 0 :: Word64
        go rmm [] = rmm

toBools :: ParensSeq -> [Bool]
toBools rmm = toBoolsDiff rmm []

toBoolsDiff :: ParensSeq -> [Bool] -> [Bool]
toBoolsDiff rmm = mconcat (fmap go (toPartialWord64s rmm))
  where go :: (Word64, Count) -> [Bool] -> [Bool]
        go (w, n) = W.partialToBoolsDiff (fromIntegral n) w

drop :: Count -> ParensSeq -> ParensSeq
drop n (ParensSeq parens) = case FT.split (T.atSizeBelowZero n) parens of
  (lt, rt) -> let n' = n - T.size (FT.measure lt :: T.Measure) in
    case FT.viewl rt of
      T.Elem w nw :< rrt -> if n' >= nw
        then ParensSeq rrt
        else ParensSeq (T.Elem (w .>. n') (nw - n') <| rrt)
      FT.EmptyL          -> empty

drop2 :: Count -> ParensSeq -> ParensSeq
drop2 n (ParensSeq parens) = case T.ftSplit (T.atSizeBelowZero n) parens of
  (_, rt) -> ParensSeq rt

firstChild  :: ParensSeq -> Count -> Maybe Count
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
  where ParensSeq ft = drop (n - 1) rmm

nextSibling  :: ParensSeq -> Count -> Maybe Count
nextSibling (ParensSeq rmm) n = do
  let (lt0, rt0) = T.ftSplit (T.atSizeBelowZero (n - 1)) rmm
  _ <- case FT.viewl rt0 of
    T.Elem w nw :< _ -> if nw >= 1 && w .&. 1 == 1 then Just () else Nothing
    FT.EmptyL        -> Nothing
  let (lt1, rt1) = T.ftSplit (T.atSizeBelowZero 1) rt0
  let (lt2, rt2) = T.ftSplit T.atMinZero  rt1
  case FT.viewl rt2 of
    T.Elem w nw :< _ -> if nw >= 1 && w .&. 1 == 0 then Just () else Nothing
    FT.EmptyL        -> Nothing
  let (lt3, rt3) = T.ftSplit (T.atSizeBelowZero 1) rt2
  case FT.viewl rt3 of
    T.Elem w nw :< _ -> if nw >= 1 && w .&. 1 == 1 then Just () else Nothing
    FT.EmptyL        -> Nothing
  return $ 1
    + T.size (FT.measure lt0 :: T.Measure)
    + T.size (FT.measure lt1 :: T.Measure)
    + T.size (FT.measure lt2 :: T.Measure)
    + T.size (FT.measure lt3 :: T.Measure)
