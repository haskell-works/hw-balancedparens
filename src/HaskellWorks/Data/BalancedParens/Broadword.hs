{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword
  ( findCloseW64
  , ocCalc8
  , showPadded
  , kkBitDiffPos
  , kkBitDiff
  , kkBitDiffSimple
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword

import qualified Data.Bits as DB

findCloseW64 :: Word64 -> Word64
findCloseW64 x =                                                                                     -- let !_ = traceW "x00" x   in
  let !b00 = x - ((x .&. 0xaaaaaaaaaaaaaaaa) .>. 1)                                               in -- let !_ = traceW "b00" b00 in
  let !b01 = (b00 .&. 0x3333333333333333) + ((b00 .>. 2) .&. 0x3333333333333333)                  in -- let !_ = traceW "b01" b01 in
  let !b02 = (b01 + (b01 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f                                           in -- let !_ = traceW "b02" b02 in
  let !b03 = (b02 * 0x0101010101010101) .<. 1                                                     in -- let !_ = traceW "b03" b03 in
  let !b04 = kBitDiffUnsafe 8 (h 8 .|. 0x4038302820181008) b03                                    in -- let !_ = traceW "b04" b04 in
  let !u00 = (((((b04 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in -- let !_ = traceW "u00" u00 in
  let !z00 =                         ((h 8 .>. 1) .|. (l 8 * 7)) .&. u00                          in -- let !_ = traceW "z00" z00 in

  let !d10 = (l 8 * 2 - (((x .>. 6) .&. (l 8 .<. 1)) + ((x .>. 5) .&. (l 8 .<. 1))))              in -- let !_ = traceW "d10" d10 in
  let !b10 = b04 - d10                                                                            in -- let !_ = traceW "b10" b10 in
  let !u10 = (((((b10 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in -- let !_ = traceW "u10" u10 in
  let !z10 = (z00 .&. comp u10) .|. (((h 8 .>. 1) .|. (l 8 * 5)) .&. u10)                         in -- let !_ = traceW "z10" z10 in

  let !d20 = (l 8 * 2 - (((x .>. 4) .&. (l 8 .<. 1)) + ((x .>. 3) .&. (l 8 .<. 1))))              in -- let !_ = traceW "d20" d20 in
  let !b20 = b10 - d20                                                                            in -- let !_ = traceW "b20" b20 in
  let !u20 = (((((b20 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in -- let !_ = traceW "u20" u20 in
  let !z20 = (z10 .&. comp u20) .|. (((h 8 .>. 1) .|. (l 8 * 3)) .&. u20)                         in -- let !_ = traceW "z20" z20 in

  let !d30 = (l 8 * 2 - (((x .>. 2) .&. (l 8 .<. 1)) + ((x .>. 1) .&. (l 8 .<. 1))))              in -- let !_ = traceW "d30" d30 in
  let !b30 = b20 - d30                                                                            in -- let !_ = traceW "b30" b30 in
  let !u30 = (((((b30 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in -- let !_ = traceW "u30" u30 in
  let !z30 = (z20 .&. comp u30) .|. (((h 8 .>. 1) .|.  l 8     ) .&. u30)                         in -- let !_ = traceW "z30" z30 in

  let !p00 = lsb (z30 .>. 6 .&. l 8)                                                              in -- let !_ = traceW "p00" p00 in
  let !r00 = ((p00 + ((z30 .>. fromIntegral (p00 .&. 0x3f)) .&. 0x3f)) .|. (p00 .>. 8)) .&. 0x7f  in -- let !_ = traceW "r00" r00 in
  r00
{-# INLINE findCloseW64 #-}

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
