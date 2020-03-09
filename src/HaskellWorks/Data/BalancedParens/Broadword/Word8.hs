{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word8
  ( ocCalc8
  , showPadded
  , kkBitDiffPos
  , kkBitDiff
  , kkBitDiffSimple
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Bits as DB

µµ1 :: Word8
µµ1 = 0x33

hh :: Int -> Word8
hh 2  = 0xaa
hh 4  = 0x88
hh 8  = 0x80
hh 16 = 0x80
hh 32 = 0x80
hh 64 = 0x80
hh k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE hh #-}

kkBitDiff :: Int -> Word8 -> Word8 -> Word8
kkBitDiff k x y = ((x .|. hh k) - (y .&. comp (hh k))) .^. ((x .^. comp y) .&. hh k)
{-# INLINE kkBitDiff #-}

kkBitDiffSimple :: Int -> Word8 -> Word8 -> Word8
kkBitDiffSimple k x y = ((x .|. hh k) - y) .^. hh k
{-# INLINE kkBitDiffSimple #-}

kkBitDiffPos :: Int -> Word8 -> Word8 -> Word8
kkBitDiffPos k x y = let d = kkBitDiff k x y in d .&. kkBitDiff k (d .>. fromIntegral (k - 1)) 1
{-# INLINE kkBitDiffPos #-}

showPadded :: Show a => Int -> a -> String
showPadded n a = reverse (take n (reverse (show a) ++ [' ', ' ' ..]))

(.>+.) :: Word8 -> Int -> Word8
(.>+.) w n = fromIntegral ((fromIntegral w :: Int8) `DB.shift` (-n))

ocCalc8 :: Word8 -> Word8 -> Word8
ocCalc8 p x =
  let b0  =   x .&. 0x55                                                            in
  let b1  =  (x .&. 0xAA) .>. 1                                                     in
  let ll  =  (b0 .^. b1) .&. b1                                                     in
  let o1  =  (b0 .&. b1)           .<. 1 .|. ll                                     in
  let c1  = ((b0 .|. b1) .^. 0x55) .<. 1 .|. ll                                     in

  -- arithmetic operators come first, ordered in the standard way
  -- followed by shifts
  -- .&.
  -- .^.
  -- .|.
  let eo1 =   o1 .&.  µµ1                                                           in
  let ec1 =  (c1 .&. (µµ1 .<.  2)) .>.  2                                           in
  let o2  = ((o1 .&. (µµ1 .<.  2)) .>.  2) + kkBitDiffPos 4 eo1 ec1                 in
  let c2  =  (c1 .&.  µµ1)                 + kkBitDiffPos 4 ec1 eo1                 in

  let bb2  = ((((c2 .>. 0) .&. 15) - p) .>+. 7)                                     in
  let mm2  = bb2 .&. 15                                                             in
  let pa2  = p   - (c2 .&. mm2)                                                     in
  let pb2  = pa2 + (o2 .&. mm2)                                                     in
  let ss2  = 4 .&. bb2                                                              in

  let bb1  = ((((c1 .>. fromIntegral ss2) .&. 3) - pb2) .>+. 7)                     in
  let mm1  = bb1 .&. 3                                                              in
  let pa1  = pa2 - (c1 .&. mm1)                                                     in
  let pb1  = pa1 + (o1 .&. mm1)                                                     in
  let ss1  = ss2 + (2  .&. bb1)                                                     in

  let rrr  = ss1 + pb1 + (((x .>. fromIntegral ss1) .&. ((pb1 .<. 1) .|. 1)) .<. 1) in

  rrr
