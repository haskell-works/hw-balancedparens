{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.ParensSeq
  ( ParensSeq(..)
  , mempty
  , size
  , fromWord64s
  , fromPartialWord64s
  , toPartialWord64s
  , fromBools
  , toBools
  , splitAt
  , take
  , drop
  , firstChild
  , nextSibling
  , (<|), (><), (|>)
  ) where

import Data.Coerce
import Data.Foldable
import Data.Monoid
import Data.Word
import HaskellWorks.Data.BalancedParens.Internal.ParensSeq (Elem (Elem), ParensSeq (ParensSeq), ParensSeqFt)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                        (ViewL (..), ViewR (..), (<|), (><), (|>))
import HaskellWorks.Data.Positioning
import Prelude                                             hiding (drop, max, min, splitAt, take)

import qualified Data.List                                           as L
import qualified HaskellWorks.Data.BalancedParens.Internal.ParensSeq as PS
import qualified HaskellWorks.Data.BalancedParens.Internal.Word      as W
import qualified HaskellWorks.Data.FingerTree                        as FT

empty :: ParensSeq
empty = ParensSeq FT.empty

size :: ParensSeq -> Count
size (ParensSeq parens) = PS.size (FT.measure parens :: PS.Measure)

-- TODO Needs optimisation
fromWord64s :: Traversable f => f Word64 -> ParensSeq
fromWord64s = foldl go empty
  where go :: ParensSeq -> Word64 -> ParensSeq
        go ps w = ParensSeq (PS.parens ps |> Elem w 64)

-- TODO Needs optimisation
fromPartialWord64s :: Traversable f => f (Word64, Count) -> ParensSeq
fromPartialWord64s = foldl go empty
  where go :: ParensSeq -> (Word64, Count) -> ParensSeq
        go ps (w, n) = ParensSeq (PS.parens ps |> Elem w n)

toPartialWord64s :: ParensSeq -> [(Word64, Count)]
toPartialWord64s = L.unfoldr go . coerce
  where go :: ParensSeqFt -> Maybe ((Word64, Count), ParensSeqFt)
        go ft = case FT.viewl ft of
          PS.Elem w n :< rt -> Just ((w, coerce n), rt)
          FT.EmptyL         -> Nothing

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
        go ps [] = ps

toBools :: ParensSeq -> [Bool]
toBools ps = toBoolsDiff ps []

toBoolsDiff :: ParensSeq -> [Bool] -> [Bool]
toBoolsDiff ps = mconcat (fmap go (toPartialWord64s ps))
  where go :: (Word64, Count) -> [Bool] -> [Bool]
        go (w, n) = W.partialToBoolsDiff (fromIntegral n) w

drop :: Count -> ParensSeq -> ParensSeq
drop n ps = snd (splitAt n ps)

take :: Count -> ParensSeq -> ParensSeq
take n ps = fst (splitAt n ps)

splitAt :: Count -> ParensSeq -> (ParensSeq, ParensSeq)
splitAt n (ParensSeq parens) = case FT.split (PS.atSizeBelowZero n) parens of
  (lt, rt) -> let
    n' = n - PS.size (FT.measure lt :: PS.Measure)
    u  = 64 - n'
    in case FT.viewl rt of
      PS.Elem w nw :< rrt -> if n' >= nw
        then (ParensSeq  lt                                 , ParensSeq                                  rrt )
        else (ParensSeq (lt |> PS.Elem ((w .<. u) .>. u) n'), ParensSeq (PS.Elem (w .>. n') (nw - n') <| rrt))
      FT.EmptyL          -> (ParensSeq lt, ParensSeq FT.empty)

firstChild  :: ParensSeq -> Count -> Maybe Count
firstChild ps n = case FT.viewl ft of
  PS.Elem w nw :< rt -> if nw >= 2
    then case w .&. 3 of
      3 -> Just (n + 1)
      _ -> Nothing
    else if nw >= 1
      then case w .&. 1 of
        1 -> case FT.viewl rt of
          PS.Elem w' nw' :< _ -> if nw' >= 1
            then case w' .&. 1 of
              1 -> Just (n + 1)
              _ -> Nothing
            else Nothing
          FT.EmptyL -> Nothing
        _ -> Nothing
      else Nothing
  FT.EmptyL -> Nothing
  where ParensSeq ft = drop (n - 1) ps

nextSibling  :: ParensSeq -> Count -> Maybe Count
nextSibling (ParensSeq ps) n = do
  let (lt0, rt0) = PS.ftSplit (PS.atSizeBelowZero (n - 1)) ps
  _ <- case FT.viewl rt0 of
    PS.Elem w nw :< _ -> if nw >= 1 && w .&. 1 == 1 then Just () else Nothing
    FT.EmptyL         -> Nothing
  let (lt1, rt1) = PS.ftSplit (PS.atSizeBelowZero 1) rt0
  let (lt2, rt2) = PS.ftSplit PS.atMinZero  rt1
  case FT.viewl rt2 of
    PS.Elem w nw :< _ -> if nw >= 1 && w .&. 1 == 0 then Just () else Nothing
    FT.EmptyL         -> Nothing
  let (lt3, rt3) = PS.ftSplit (PS.atSizeBelowZero 1) rt2
  case FT.viewl rt3 of
    PS.Elem w nw :< _ -> if nw >= 1 && w .&. 1 == 1 then Just () else Nothing
    FT.EmptyL         -> Nothing
  return $ 1
    + PS.size (FT.measure lt0 :: PS.Measure)
    + PS.size (FT.measure lt1 :: PS.Measure)
    + PS.size (FT.measure lt2 :: PS.Measure)
    + PS.size (FT.measure lt3 :: PS.Measure)
