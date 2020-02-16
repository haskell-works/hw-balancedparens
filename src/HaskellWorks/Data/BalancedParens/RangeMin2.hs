{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.BalancedParens.RangeMin2
  ( RangeMin2(..)
  , mkRangeMin2
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

import qualified Data.Vector                                               as DV
import qualified Data.Vector.Storable                                      as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Vector.Storable as DVS

data RangeMin2 a = RangeMin2
  { rangeMin2BP       :: !a
  , rangeMin2L0Min    :: !(DVS.Vector Int8)
  , rangeMin2L0Excess :: !(DVS.Vector Int8)
  , rangeMin2L1Min    :: !(DVS.Vector Int16)
  , rangeMin2L1Excess :: !(DVS.Vector Int16)
  , rangeMin2L2Min    :: !(DVS.Vector Int16)
  , rangeMin2L2Excess :: !(DVS.Vector Int16)
  , rangeMin2L3Min    :: !(DVS.Vector Int16)
  , rangeMin2L3Excess :: !(DVS.Vector Int16)
  , rangeMin2L4Min    :: !(DVS.Vector Int16)
  , rangeMin2L4Excess :: !(DVS.Vector Int16)
  } deriving (NFData, Generic)

factorL0 :: Integral a => a
factorL0 = 1
{-# INLINE factorL0 #-}

factorL1 :: Integral a => a
factorL1 = 32
{-# INLINE factorL1 #-}

factorL2 :: Integral a => a
factorL2 = 32
{-# INLINE factorL2 #-}

factorL3 :: Integral a => a
factorL3 = 32
{-# INLINE factorL3 #-}

factorL4 :: Integral a => a
factorL4 = 32
{-# INLINE factorL4 #-}

pageSizeL0 :: Integral a => a
pageSizeL0 = factorL0
{-# INLINE pageSizeL0 #-}

pageSizeL1 :: Integral a => a
pageSizeL1 = pageSizeL0 * factorL1
{-# INLINE pageSizeL1 #-}

pageSizeL2 :: Integral a => a
pageSizeL2 = pageSizeL1 * factorL2
{-# INLINE pageSizeL2 #-}

pageSizeL3 :: Integral a => a
pageSizeL3 = pageSizeL2 * factorL3
{-# INLINE pageSizeL3 #-}

pageSizeL4 :: Integral a => a
pageSizeL4 = pageSizeL3 * factorL4
{-# INLINE pageSizeL4 #-}

mkRangeMin2 :: AsVector64 a => a -> RangeMin2 a
mkRangeMin2 bp = RangeMin2
  { rangeMin2BP       = bp
  , rangeMin2L0Min    = DVS.reword rmL0Min
  , rangeMin2L0Excess = DVS.reword rmL0Excess
  , rangeMin2L1Min    = rmL1Min
  , rangeMin2L1Excess = rmL1Excess
  , rangeMin2L2Min    = rmL2Min
  , rangeMin2L2Excess = rmL2Excess
  , rangeMin2L3Min    = rmL3Min
  , rangeMin2L3Excess = rmL3Excess
  , rangeMin2L4Min    = rmL4Min
  , rangeMin2L4Excess = rmL4Excess
  }
  where bpv           = asVector64 bp
        lenBP         = fromIntegral (length bpv) :: Int
        lenL0         = lenBP
        lenL1         = (DVS.length rmL0Min `div` pageSizeL1) + 1 :: Int
        lenL2         = (DVS.length rmL0Min `div` pageSizeL2) + 1 :: Int
        lenL3         = (DVS.length rmL0Min `div` pageSizeL3) + 1 :: Int
        lenL4         = (DVS.length rmL0Min `div` pageSizeL4) + 1 :: Int
        allMinL0      = DV.generate  lenL0 (\i -> if i == lenBP then MinExcess (-64) (-64) else minExcess1 (bpv !!! fromIntegral i))
        -- Note: (0xffffffffffffffc0 :: Int64) = -64
        rmL0Excess    = DVS.generate lenL0 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL0 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL1Excess    = DVS.generate lenL1 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL1 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL2Excess    = DVS.generate lenL2 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL2 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL3Excess    = DVS.generate lenL3 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL3 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL4Excess    = DVS.generate lenL4 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL4 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL0Min       = DVS.generate lenL0 (\i -> let MinExcess minE _ = allMinL0 DV.! i in fromIntegral minE) :: DVS.Vector Int16
        rmL1Min       = DVS.generate lenL1 (\i -> genMin 0 (DVS.pageFill i factorL1 0 rmL0Min) (DVS.pageFill i factorL1 0 rmL0Excess))
        rmL2Min       = DVS.generate lenL2 (\i -> genMin 0 (DVS.pageFill i factorL2 0 rmL1Min) (DVS.pageFill i factorL2 0 rmL1Excess))
        rmL3Min       = DVS.generate lenL3 (\i -> genMin 0 (DVS.pageFill i factorL3 0 rmL2Min) (DVS.pageFill i factorL3 0 rmL2Excess))
        rmL4Min       = DVS.generate lenL4 (\i -> genMin 0 (DVS.pageFill i factorL4 0 rmL3Min) (DVS.pageFill i factorL4 0 rmL3Excess))

genMin :: (Integral a, DVS.Storable a) => a -> DVS.Vector a -> DVS.Vector a -> a
genMin mL mins excesses = if not (DVS.null mins) || not (DVS.null excesses)
  then genMin (DVS.lastOrZero mins `min` (mL + DVS.lastOrZero excesses)) (DVS.init mins) (DVS.init excesses)
  else mL

data FindState = FindBP
  | FindL0 | FindFromL0
  | FindL1 | FindFromL1
  | FindL2 | FindFromL2
  | FindL3 | FindFromL3
  | FindL4 | FindFromL4

rm2FindClose  :: (BitLength a, NewCloseAt a) => RangeMin2 a -> Int -> Count -> FindState -> Maybe Count
rm2FindClose v s p FindBP = if v `newCloseAt` p
  then if s <= 1
    then Just p
    else rm2FindClose v (s - 1) (p + 1) FindFromL0
  else rm2FindClose v (s + 1) (p + 1) FindFromL0
rm2FindClose v s p FindL0 =
  let i = p `div` 64 in
  let mins = rangeMin2L0Min v in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindBP
    else if v `newCloseAt` p && s <= 1
      then Just p
      else  let excesses  = rangeMin2L0Excess v in
            let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
rm2FindClose v s p FindL1 =
  let !i = p `div` (64 * pageSizeL1) in
  let !mins = rangeMin2L1Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindL0
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMin2L1Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL1)) FindFromL1
      else Nothing
rm2FindClose v s p FindL2 =
  let !i = p `div` (64 * pageSizeL2) in
  let !mins = rangeMin2L2Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindL1
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMin2L2Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL2)) FindFromL2
      else Nothing
rm2FindClose v s p FindL3 =
  let !i = p `div` (64 * pageSizeL3) in
  let !mins = rangeMin2L3Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindL2
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMin2L3Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL3)) FindFromL3
        else Nothing
rm2FindClose v s p FindL4 =
  let !i = p `div` (64 * pageSizeL4) in
  let !mins = rangeMin2L4Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rm2FindClose v s p FindL3
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMin2L4Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL4)) FindFromL4
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
  | p `mod` (64 * pageSizeL2) == 0  = if 0 <= p && p < bitLength v then rm2FindClose v s p FindFromL3 else Nothing
  | 0 <= p && p < bitLength v       = rm2FindClose v s p FindL1
  | otherwise                       = Nothing
rm2FindClose v s p FindFromL3
  | p `mod` (64 * pageSizeL3) == 0  = if 0 <= p && p < bitLength v then rm2FindClose v s p FindFromL4 else Nothing
  | 0 <= p && p < bitLength v       = rm2FindClose v s p FindL2
  | otherwise                       = Nothing
rm2FindClose v s p FindFromL4
  | p `mod` (64 * pageSizeL4) == 0  = if 0 <= p && p < bitLength v then rm2FindClose v s p FindL4 else Nothing
  | 0 <= p && p < bitLength v       = rm2FindClose v s p FindL3
  | otherwise                       = Nothing
{-# INLINE rm2FindClose #-}

instance TestBit a => TestBit (RangeMin2 a) where
  (.?.) = (.?.) . rangeMin2BP
  {-# INLINE (.?.) #-}

instance Rank1 a => Rank1 (RangeMin2 a) where
  rank1 = rank1 . rangeMin2BP
  {-# INLINE rank1 #-}

instance Rank0 a => Rank0 (RangeMin2 a) where
  rank0 = rank0 . rangeMin2BP
  {-# INLINE rank0 #-}

instance BitLength a => BitLength (RangeMin2 a) where
  bitLength = bitLength . rangeMin2BP
  {-# INLINE bitLength #-}

instance OpenAt a => OpenAt (RangeMin2 a) where
  openAt = openAt . rangeMin2BP
  {-# INLINE openAt #-}

instance CloseAt a => CloseAt (RangeMin2 a) where
  closeAt = closeAt . rangeMin2BP
  {-# INLINE closeAt #-}

instance NewCloseAt a => NewCloseAt (RangeMin2 a) where
  newCloseAt = newCloseAt . rangeMin2BP
  {-# INLINE newCloseAt #-}

instance FindOpenN a => FindOpenN (RangeMin2 a) where
  findOpenN = findOpenN . rangeMin2BP
  {-# INLINE findOpenN #-}

instance (BitLength a, FindCloseN a, NewCloseAt a) => FindCloseN (RangeMin2 a) where
  findCloseN v s p  = (+ 1) `fmap` rm2FindClose v (fromIntegral s) (p - 1) FindFromL0
  {-# INLINE findCloseN  #-}

instance (BitLength a, NewCloseAt a, CloseAt a, FindCloseN a) => FindClose (RangeMin2 a) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance (OpenAt a, FindOpenN a) => FindOpen (RangeMin2 a) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpenN a => Enclose (RangeMin2 a) where
  enclose v = findOpenN v 1
  {-# INLINE enclose #-}

instance (BitLength a, NewCloseAt a, CloseAt a, OpenAt a, FindOpenN a, FindCloseN a) => BalancedParens (RangeMin2 a)
