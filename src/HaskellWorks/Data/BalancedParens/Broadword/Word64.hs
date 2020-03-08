{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word64
  ( findClose
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word64
import HaskellWorks.Data.Bits.Word64

-- Source:
--    Broadword Implementation of Parenthesis Queries
--    Sebastiano Vigna
--    Dipartimento di Scienze dell’Informazione
--    Università degli Studi di Milano, Italy
findClose :: Word64 -> Word64
findClose x =                                                                                     -- let !_ = traceW "x00" x   in
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
{-# INLINE findClose #-}
