{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.BalancedParens.RangeMin
  ( RangeMin(..)
  , mkRangeMin
  ) where

import Control.DeepSeq
import Data.Int
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.BalancedParens.Enclose
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.BalancedParens.FindCloseN
import HaskellWorks.Data.BalancedParens.FindOpen
import HaskellWorks.Data.BalancedParens.FindOpenN
import HaskellWorks.Data.BalancedParens.NewCloseAt
import HaskellWorks.Data.BalancedParens.OpenAt
import HaskellWorks.Data.Bits.AllExcess.AllExcess1
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.MinExcess
import HaskellWorks.Data.Excess.MinExcess1
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                         hiding (length)

import qualified Data.Vector.Storable                                      as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Vector.Storable as DVS

data RangeMin a = RangeMin
  { rangeMinBP       :: !a
  , rangeMinL0Min    :: !(DVS.Vector Int8)
  , rangeMinL0Excess :: !(DVS.Vector Int8)
  , rangeMinL1Min    :: !(DVS.Vector Int16)
  , rangeMinL1Excess :: !(DVS.Vector Int16)
  , rangeMinL2Min    :: !(DVS.Vector Int16)
  , rangeMinL2Excess :: !(DVS.Vector Int16)
  } deriving (Eq, Show, NFData, Generic)

factorL0 :: Integral a => a
factorL0 = 1
{-# INLINE factorL0 #-}

factorL1 :: Integral a => a
factorL1 = 32
{-# INLINE factorL1 #-}

factorL2 :: Integral a => a
factorL2 = 32
{-# INLINE factorL2 #-}

pageSizeL0 :: Integral a => a
pageSizeL0 = factorL0
{-# INLINE pageSizeL0 #-}

pageSizeL1 :: Integral a => a
pageSizeL1 = pageSizeL0 * factorL1
{-# INLINE pageSizeL1 #-}

pageSizeL2 :: Integral a => a
pageSizeL2 = pageSizeL1 * factorL2
{-# INLINE pageSizeL2 #-}

mkRangeMin :: AsVector64 a => a -> RangeMin a
mkRangeMin bp = RangeMin
  { rangeMinBP       = bp
  , rangeMinL0Min    = rmL0Min
  , rangeMinL0Excess = DVS.reword rmL0Excess
  , rangeMinL1Min    = rmL1Min
  , rangeMinL1Excess = DVS.reword rmL1Excess
  , rangeMinL2Min    = rmL2Min
  , rangeMinL2Excess = rmL2Excess
  }
  where bpv           = asVector64 bp
        lenBP         = fromIntegral (length bpv) :: Int
        lenL0         = lenBP
        lenL1         = (DVS.length rmL0Min `div` pageSizeL1) + 1 :: Int
        lenL2         = (DVS.length rmL0Min `div` pageSizeL2) + 1 :: Int
        allMinL0      = DVS.generate lenL0 (\i -> if i == lenBP then MinExcess (-64) (-64) else minExcess1 (bpv !!! fromIntegral i))
        allMinL1      = DVS.generate lenL1 (\i -> minExcess1 (DVS.dropTake (i * pageSizeL1) pageSizeL1 bpv))
        allMinL2      = DVS.generate lenL2 (\i -> minExcess1 (DVS.dropTake (i * pageSizeL2) pageSizeL2 bpv))
        -- Note: (0xffffffffffffffc0 :: Int64) = -64
        rmL0Excess    = DVS.generate lenL0 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL0 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL1Excess    = DVS.generate lenL1 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL1 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL2Excess    = DVS.generate lenL2 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL2 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL0Min       = DVS.generate lenL0 (\i -> let MinExcess minE _ = allMinL0 DVS.! i in fromIntegral minE)
        rmL1Min       = DVS.generate lenL1 (\i -> let MinExcess minE _ = allMinL1 DVS.! i in fromIntegral minE)
        rmL2Min       = DVS.generate lenL2 (\i -> let MinExcess minE _ = allMinL2 DVS.! i in fromIntegral minE)

data FindState = FindBP
  | FindL0 | FindFromL0
  | FindL1 | FindFromL1
  | FindL2 | FindFromL2

rm2FindClose  :: (BitLength a, NewCloseAt a) => RangeMin a -> Int -> Count -> FindState -> Maybe Count
rm2FindClose v s p FindBP = if v `newCloseAt` p
  then if s <= 1
    then Just p
    else rm2FindClose v (s - 1) (p + 1) FindFromL0
  else rm2FindClose v (s + 1) (p + 1) FindFromL0
rm2FindClose v s p FindL0 =
  let i = p `div` 64 in
  let mins = rangeMinL0Min v in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindBP
    else if v `newCloseAt` p && s <= 1
      then Just p
      else  let excesses  = rangeMinL0Excess v in
            let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
rm2FindClose v s p FindL1 =
  let !i = p `div` (64 * pageSizeL1) in
  let !mins = rangeMinL1Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindL0
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinL1Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL1)) FindFromL1
      else Nothing
rm2FindClose v s p FindL2 =
  let !i = p `div` (64 * pageSizeL2) in
  let !mins = rangeMinL2Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindL1
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinL2Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL2)) FindFromL2
      else Nothing
rm2FindClose v s p FindFromL0
  | p `mod` 64 == 0             = rm2FindClose v s p FindFromL1
  | 0 <= p && p < bitLength v   = rm2FindClose v s p FindBP
  | otherwise                   = Nothing
rm2FindClose v s p FindFromL1
  | p `mod` (64 * pageSizeL1) == 0  = if 0 <= p && p < bitLength v then rm2FindClose v s p FindFromL2 else Nothing
  | 0 <= p && p < bitLength v       = rm2FindClose v s p FindL0
  | otherwise                       = Nothing
rm2FindClose v s p FindFromL2
  | p `mod` (64 * pageSizeL2) == 0  = if 0 <= p && p < bitLength v then rm2FindClose v s p FindL2 else Nothing
  | 0 <= p && p < bitLength v       = rm2FindClose v s p FindL1
  | otherwise                       = Nothing
{-# INLINE rm2FindClose #-}

instance TestBit a => TestBit (RangeMin a) where
  (.?.) = (.?.) . rangeMinBP
  {-# INLINE (.?.) #-}

instance Rank1 a => Rank1 (RangeMin a) where
  rank1 = rank1 . rangeMinBP
  {-# INLINE rank1 #-}

instance Rank0 a => Rank0 (RangeMin a) where
  rank0 = rank0 . rangeMinBP
  {-# INLINE rank0 #-}

instance BitLength a => BitLength (RangeMin a) where
  bitLength = bitLength . rangeMinBP
  {-# INLINE bitLength #-}

instance OpenAt a => OpenAt (RangeMin a) where
  openAt = openAt . rangeMinBP
  {-# INLINE openAt #-}

instance CloseAt a => CloseAt (RangeMin a) where
  closeAt = closeAt . rangeMinBP
  {-# INLINE closeAt #-}

instance NewCloseAt a => NewCloseAt (RangeMin a) where
  newCloseAt = newCloseAt . rangeMinBP
  {-# INLINE newCloseAt #-}

instance FindOpenN a => FindOpenN (RangeMin a) where
  findOpenN = findOpenN . rangeMinBP
  {-# INLINE findOpenN #-}

instance (BitLength a, NewCloseAt a) => FindCloseN (RangeMin a) where
  findCloseN v s p  = (+ 1) `fmap` rm2FindClose v (fromIntegral s) (p - 1) FindFromL0
  {-# INLINE findCloseN  #-}

instance (BitLength a, CloseAt a, NewCloseAt a, FindCloseN a) => FindClose (RangeMin a) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance (OpenAt a, FindOpenN a) => FindOpen (RangeMin a) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpenN a => Enclose (RangeMin a) where
  enclose v = findOpenN v 1
  {-# INLINE enclose #-}

instance (BitLength a, NewCloseAt a, CloseAt a, OpenAt a, FindOpenN a, FindCloseN a) => BalancedParens (RangeMin a)
