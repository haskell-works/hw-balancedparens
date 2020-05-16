{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word8
  ( findUnmatchedCloseFar
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word8
import HaskellWorks.Data.Int.Narrow
import HaskellWorks.Data.Int.Widen
import HaskellWorks.Data.Positioning

muk1 :: Word8
muk1 = 0x33
{-# INLINE muk1 #-}

muk2 :: Word8
muk2 = 0x0f
{-# INLINE muk2 #-}

-- | Find the position of the first unmatch parenthesis.
--
-- This is the broadword implementation of 'HaskellWorks.Data.BalancedParens.Internal.Slow.Word8.findCloseFor'.
--
-- See [Broadword Implementation of Parenthesis Queries](https://arxiv.org/pdf/1301.5468.pdf), Sebastiano Vigna, 2013
findUnmatchedCloseFar :: Word64 -> Word64 -> Word8 -> Word64
findUnmatchedCloseFar c p w =
  --  Keys:
  --    * k1: Level of sub-words of size 2 = 1 .<. 1
  --    * k2: Level of sub-words of size 4 = 1 .<. 2
  --    * k3: Level of sub-words of size 8 = 1 .<. 3
  --    * o: Open count
  --    * c: Close count
  --    * e: Excess
  --    * L: Left half of sub-word
  --    * R: Right half of sub-word
  --    * b: deficit at position whole-word mask
  --    * m: deficit at position sub-word mask
  --    * pa: position accumulator
  --    * sa: shift accumulator
  --    * h: high sub-block close parens count
  --    * f: far sub-block close parens count
  let x     = w .>. p                                                                           in
  let wsz   = 8 :: Int8                                                                         in
  let k1    = 1                                                                                 in
  let k2    = 2                                                                                 in
  let k3    = 3                                                                                 in
  let mask3 = (1 .<. (1 .<. k3)) - 1                                                            in
  let mask2 = (1 .<. (1 .<. k2)) - 1                                                            in
  let mask1 = (1 .<. (1 .<. k1)) - 1                                                            in
  let t64k1 = 1 .<. k1 :: Word64                                                                in
  let t64k2 = 1 .<. k2 :: Word64                                                                in
  let t8k1  = 1 .<. k1 :: Word8                                                                 in
  let t8k2  = 1 .<. k2 :: Word8                                                                 in
  let t8k3  = 1 .<. k3 :: Word8                                                                 in

  let b0    =      x .&. 0x55                                                                   in
  let b1    =    ( x .&. 0xaa) .>. 1                                                            in
  let ll    =   (b0  .^. b1  ) .&. b1                                                           in
  let ok1   = ( (b0  .&. b1  )           .<. 1) .|. ll                                          in
  let ck1   = (((b0  .|. b1  ) .^. 0x55) .<. 1) .|. ll                                          in

  let eok1  =   ok1 .&.  muk1                                                                   in
  let eck1  =  (ck1 .&. (muk1 .<. t64k1)) .>. t64k1                                             in
  let ok2L  =  (ok1 .&. (muk1 .<. t64k1)) .>. t64k1                                             in
  let ok2R  = kBitDiffPos 4 eok1 eck1                                                           in
  let ok2   = ok2L + ok2R                                                                       in
  let ck2   =  (ck1 .&.  muk1) + kBitDiffPos 4 eck1 eok1                                        in

  let eok2  =   ok2 .&.  muk2                                                                   in
  let eck2  =  (ck2 .&. (muk2 .<. t64k2)) .>. t64k2                                             in
  let ok3L  =  (ok2 .&. (muk2 .<. t64k2)) .>. t64k2                                             in
  let ok3R  = kBitDiffPos 8 eok2 eck2                                                           in
  let ok3   = ok3L + ok3R                                                                       in
  let ck3   =  (ck2 .&.  muk2) + kBitDiffPos 8 eck2 eok2                                        in

  let pak3  = c                                                                                 in
  let sak3  = 0                                                                                 in

  let hk3   = 0x08 .&. comp (0xff .>. fromIntegral sak3)                                        in
  let fk3   = (ck3 .>. fromIntegral sak3 .|. hk3) .&. mask3                                     in
  let bk3   = ((narrow pak3 - fk3) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk3   = bk3 .&. mask3                                                                     in
  let pbk3  = pak3 - widen (((ck3 .>. fromIntegral sak3) .|. hk3) .&. mk3)                      in
  let pck3  = pbk3 + widen (((ok3 .>. fromIntegral sak3) .|. hk3) .&. mk3)                      in
  let sbk3  = sak3 + widen (t8k3 .&. bk3)                                                       in

  let pak2  = pck3                                                                              in
  let sak2  = sbk3                                                                              in

  let hk2   = 0x44 .&. comp (0xff .>. fromIntegral sak2)                                        in
  let fk2   = ((ck2 .>. fromIntegral sak2) .|. hk2) .&. mask2                                   in
  let bk2   = ((narrow pak2 - fk2) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk2   = bk2 .&. mask2                                                                     in
  let pbk2  = pak2 - widen (((ck2 .>. fromIntegral sak2) .|. hk2) .&. mk2)                      in
  let pck2  = pbk2 + widen ( (ok2 .>. fromIntegral sak2)          .&. mk2)                      in
  let sbk2  = sak2 + widen (t8k2 .&. bk2)                                                       in

  let pak1  = pck2                                                                              in
  let sak1  = sbk2                                                                              in

  let hk1   = 0xaa .&. comp (0xff .>. fromIntegral sak1)                                        in
  let fk1   = ((ck1 .>. fromIntegral sak1) .|. hk1) .&. mask1                                   in
  let bk1   = ((narrow pak1 - fk1) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk1   = bk1  .&. mask1                                                                    in
  let pbk1  = pak1 - widen (((ck1 .>. fromIntegral sak1) .|. hk1) .&. mk1)                      in
  let pck1  = pbk1 + widen ( (ok1 .>. fromIntegral sak1)          .&. mk1)                      in
  let sbk1  = sak1 + widen (t8k1 .&. bk1)                                                       in

  let rrr   = sbk1 + pck1 + (((widen x .>. fromIntegral sbk1) .&. ((pck1 .<. 1) .|. 1)) .<. 1)  in

  rrr + p
{-# INLINE findUnmatchedCloseFar #-}
