{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.Word64
  ( findUnmatchedClose
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word64
import HaskellWorks.Data.Bits.Word64

-- | Find the position of the first unmatch parenthesis within a word.
--
-- See [Broadword Implementation of Parenthesis Queries](https://arxiv.org/pdf/1301.5468.pdf), Sebastiano Vigna, 2013
findUnmatchedClose :: Word64 -> Word64
findUnmatchedClose x =
  let !b00 = x - ((x .&. 0xaaaaaaaaaaaaaaaa) .>. 1)                                               in
  let !b01 = (b00 .&. 0x3333333333333333) + ((b00 .>. 2) .&. 0x3333333333333333)                  in
  let !b02 = (b01 + (b01 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f                                           in
  let !b03 = (b02 * 0x0101010101010101) .<. 1                                                     in
  let !b04 = kBitDiffUnsafe 8 (h 8 .|. 0x4038302820181008) b03                                    in
  let !u00 = (((((b04 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in
  let !z00 =                         ((h 8 .>. 1) .|. (l 8 * 7)) .&. u00                          in

  let !d10 = (l 8 * 2 - (((x .>. 6) .&. (l 8 .<. 1)) + ((x .>. 5) .&. (l 8 .<. 1))))              in
  let !b10 = b04 - d10                                                                            in
  let !u10 = (((((b10 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in
  let !z10 = (z00 .&. comp u10) .|. (((h 8 .>. 1) .|. (l 8 * 5)) .&. u10)                         in

  let !d20 = (l 8 * 2 - (((x .>. 4) .&. (l 8 .<. 1)) + ((x .>. 3) .&. (l 8 .<. 1))))              in
  let !b20 = b10 - d20                                                                            in
  let !u20 = (((((b20 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in
  let !z20 = (z10 .&. comp u20) .|. (((h 8 .>. 1) .|. (l 8 * 3)) .&. u20)                         in

  let !d30 = (l 8 * 2 - (((x .>. 2) .&. (l 8 .<. 1)) + ((x .>. 1) .&. (l 8 .<. 1))))              in
  let !b30 = b20 - d30                                                                            in
  let !u30 = (((((b30 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                              in
  let !z30 = (z20 .&. comp u30) .|. (((h 8 .>. 1) .|.  l 8     ) .&. u30)                         in

  let !p00 = lsb (z30 .>. 6 .&. l 8)                                                              in
  let !r00 = ((p00 + ((z30 .>. fromIntegral (p00 .&. 0x3f)) .&. 0x3f)) .|. (p00 .>. 8)) .&. 0x7f  in
  r00
{-# INLINE findUnmatchedClose #-}
