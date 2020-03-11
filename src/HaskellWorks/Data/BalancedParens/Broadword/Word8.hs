{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word8
  ( findCloseFar
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word8

muk1 :: Word8
muk1 = 0x33
{-# INLINE muk1 #-}

muk2 :: Word8
muk2 = 0x0f
{-# INLINE muk2 #-}

findCloseFar :: Word8 -> Word8 -> Word8
findCloseFar p x =                                                                            -- let !_ = traceW ("findCloseFar " <> show p <> " " <> bitShow x) (p, x) in
  let w     = 8 :: Int8                                                                   in  -- let !_ = traceW ("w     = " <> bitShow w    ) w     in
  let k1    = 1                                                                           in  -- let !_ = traceW ("k1    = " <> bitShow k1   ) k1    in
  let k2    = 2                                                                           in  -- let !_ = traceW ("k2    = " <> bitShow k2   ) k2    in
  let k3    = 3                                                                           in  -- let !_ = traceW ("k3    = " <> bitShow k3   ) k3    in
  let mask3 = (1 .<. (1 .<. k3)) - 1                                                      in  -- let !_ = traceW ("mask3 = " <> bitShow mask3) mask3 in
  let mask2 = (1 .<. (1 .<. k2)) - 1                                                      in  -- let !_ = traceW ("mask2 = " <> bitShow mask2) mask2 in
  let mask1 = (1 .<. (1 .<. k1)) - 1                                                      in  -- let !_ = traceW ("mask1 = " <> bitShow mask1) mask1 in
  let t64k1 = 1 .<. k1 :: Word64                                                          in  -- let !_ = traceW ("t64k1 = " <> bitShow t64k1) t64k1 in
  let t64k2 = 1 .<. k2 :: Word64                                                          in  -- let !_ = traceW ("t64k2 = " <> bitShow t64k2) t64k2 in
  let t8k1  = 1 .<. k1 :: Word8                                                           in  -- let !_ = traceW ("t8k1  = " <> bitShow t8k1 ) t8k1  in
  let t8k2  = 1 .<. k2 :: Word8                                                           in  -- let !_ = traceW ("t8k2  = " <> bitShow t8k2 ) t8k2  in
  let t8k3  = 1 .<. k3 :: Word8                                                           in  -- let !_ = traceW ("t8k3  = " <> bitShow t8k3 ) t8k3  in

  let b0    =      x .&. 0x55                                                             in  -- let !_ = traceW ("b0    = " <> bitShow b0   ) b0    in
  let b1    =    ( x .&. 0xaa) .>. 1                                                      in  -- let !_ = traceW ("b1    = " <> bitShow b1   ) b1    in
  let ll    =   (b0  .^. b1  ) .&. b1                                                     in  -- let !_ = traceW ("ll    = " <> bitShow ll   ) ll    in
  let ok1   = ( (b0  .&. b1  )           .<. 1) .|. ll                                    in  -- let !_ = traceW ("ok1   = " <> bitShow ok1  ) ok1   in
  let ck1   = (((b0  .|. b1  ) .^. 0x55) .<. 1) .|. ll                                    in  -- let !_ = traceW ("ck1   = " <> bitShow ck1  ) ck1   in

  let eok1 =   ok1 .&.  muk1                                                              in  -- let !_ = traceW ("eok1  = " <> bitShow eok1 ) eok1  in
  let eck1 =  (ck1 .&. (muk1 .<. t64k1)) .>. t64k1                                        in  -- let !_ = traceW ("eck1  = " <> bitShow eck1 ) eck1  in
  let ok2L =  (ok1 .&. (muk1 .<. t64k1)) .>. t64k1                                        in  -- let !_ = traceW ("ok2L  = " <> bitShow ok2L ) ok2L  in
  let ok2R = kBitDiffPos 4 eok1 eck1                                                      in  -- let !_ = traceW ("ok2R  = " <> bitShow ok2R ) ok2R  in
  let ok2  = ok2L + ok2R                                                                  in  -- let !_ = traceW ("ok2   = " <> bitShow ok2  ) ok2   in
  let ck2  =  (ck1 .&.  muk1) + kBitDiffPos 4 eck1 eok1                                   in  -- let !_ = traceW ("ck2   = " <> bitShow ck2  ) ck2   in

  let eok2 =   ok2 .&.  muk2                                                              in  -- let !_ = traceW ("eok2  = " <> bitShow eok2 ) eok2  in
  let eck2 =  (ck2 .&. (muk2 .<. t64k2)) .>. t64k2                                        in  -- let !_ = traceW ("eck2  = " <> bitShow eck2 ) eck2  in
  let ok3L = ((ok2 .&. (muk2 .<. t64k2)) .>. t64k2)                                       in  -- let !_ = traceW ("ok3L  = " <> bitShow ok3L ) ok3L  in
  let ok3R = kBitDiffPos 8 eok2 eck2                                                      in  -- let !_ = traceW ("ok3R  = " <> bitShow ok3R ) ok3R  in
  let ok3  = ok3L + ok3R                                                                  in  -- let !_ = traceW ("ok3   = " <> bitShow ok3  ) ok3   in
  let ck3  =  (ck2 .&.  muk2) + kBitDiffPos 8 eck2 eok2                                   in  -- let !_ = traceW ("ck3   = " <> bitShow ck3  ) ck3   in

  let pak3  = p                                                                           in  -- let !_ = traceW ("pak3  = " <> bitShow pak3 ) pak3  in
  let sak3  = 0                                                                           in  -- let !_ = traceW ("sak3  = " <> bitShow sak3 ) sak3  in

  let fk3   = (ck3 .>. fromIntegral sak3) .&. mask3                                       in  -- let !_ = traceW ("fk3   = " <> bitShow fk3  ) fk3   in
  let gk3   = pak3 - fk3                                                                  in  -- let !_ = traceW ("gk3   = " <> bitShow gk3  ) gk3   in
  let bak3  = gk3 .>. fromIntegral (w - 1)                                                in  -- let !_ = traceW ("bak3  = " <> bitShow bak3 ) bak3  in
  let bk3   = bak3 - 1                                                                    in  -- let !_ = traceW ("bk3   = " <> bitShow bk3  ) bk3   in
  let mk3   = bk3 .&. mask3                                                               in  -- let !_ = traceW ("mk3   = " <> bitShow mk3  ) mk3   in
  let pbk3  = pak3 - ((ck3 .>. fromIntegral sak3) .&. mk3)                                in  -- let !_ = traceW ("pbk3  = " <> bitShow pbk3 ) pbk3  in
  let pck3  = pbk3 + ((ok3 .>. fromIntegral sak3) .&. mk3)                                in  -- let !_ = traceW ("pck3  = " <> bitShow pck3 ) pck3  in
  let sb3   = sak3 + (t8k3 .&. bk3)                                                       in  -- let !_ = traceW ("sb3   = " <> bitShow sb3  ) sb3   in

  let pak2  = pck3                                                                        in  -- let !_ = traceW ("pak2  = " <> bitShow pak2 ) pak2  in
  let sak2  = sb3                                                                         in  -- let !_ = traceW ("sak2  = " <> bitShow sak2 ) sak2  in

  let ek2   = 0xaa .&. comp (0xff .>. fromIntegral sak2)                                  in  -- let !_ = traceW ("ek2   = " <> bitShow ek2  ) ek2   in
  let fk2   = ((ck2 .>. fromIntegral sak2) .|. ek2) .&. mask2                             in  -- let !_ = traceW ("fk2   = " <> bitShow fk2  ) fk2   in
  let gk2   = pak2 - fk2                                                                  in  -- let !_ = traceW ("gk2   = " <> bitShow gk2  ) gk2   in
  let bak2  = gk2 .>. fromIntegral (w - 1)                                                in  -- let !_ = traceW ("bak2  = " <> bitShow bak2 ) bak2  in
  let bk2   = bak2 - 1                                                                    in  -- let !_ = traceW ("bk2   = " <> bitShow bk2  ) bk2   in
  let mk2   = bk2 .&. mask2                                                               in  -- let !_ = traceW ("mk2   = " <> bitShow mk2  ) mk2   in
  let pc2   = ((ck2 .>. fromIntegral sak2) .|. ek2) .&. mk2                               in  -- let !_ = traceW ("pc2   = " <> bitShow pc2  ) pc2   in
  let po2   =  (ok2 .>. fromIntegral sak2)          .&. mk2                               in  -- let !_ = traceW ("po2   = " <> bitShow po2  ) po2   in
  let pbk2  = pak2 - pc2                                                                  in  -- let !_ = traceW ("pbk2  = " <> bitShow pbk2 ) pbk2  in
  let pck2  = pbk2 + po2                                                                  in  -- let !_ = traceW ("pck2  = " <> bitShow pck2 ) pck2  in
  let sb2   = sak2 + (t8k2 .&. bk2)                                                       in  -- let !_ = traceW ("sb2   = " <> bitShow sb2  ) sb2   in

  let pak1  = pck2                                                                        in  -- let !_ = traceW ("pak1  = " <> bitShow pak1 ) pak1  in
  let sak1  = sb2                                                                         in  -- let !_ = traceW ("sak1  = " <> bitShow sak1 ) sak1  in

  let ek1   = 0xaa .&. comp (0xff .>. fromIntegral sak1)                                  in  -- let !_ = traceW ("ek1   = " <> bitShow ek1  ) ek1   in
  let fk1   = ((ck1 .>. fromIntegral sak1) .|. ek1) .&. mask1                             in  -- let !_ = traceW ("fk1   = " <> bitShow fk1  ) fk1   in
  let gk1   = pak1 - fk1                                                                  in  -- let !_ = traceW ("gk1   = " <> bitShow gk1  ) gk1   in
  let bk1   = (gk1 .>. fromIntegral (w - 1)) - 1                                          in  -- let !_ = traceW ("bk1   = " <> bitShow bk1  ) bk1   in
  let mk1   = bk1  .&. mask1                                                              in  -- let !_ = traceW ("mk1   = " <> bitShow mk1  ) mk1   in
  let pc1   = ((ck1 .>. fromIntegral sak1) .|. ek1) .&. mk1                               in  -- let !_ = traceW ("pc1   = " <> bitShow pc1  ) pc1   in
  let po1   =  (ok1 .>. fromIntegral sak1)          .&. mk1                               in  -- let !_ = traceW ("po1   = " <> bitShow po1  ) po1   in
  let pbk1  = pak1 - pc1                                                                  in  -- let !_ = traceW ("pbk1  = " <> bitShow pbk1 ) pbk1  in
  let pck1  = pbk1 + po1                                                                  in  -- let !_ = traceW ("pck1  = " <> bitShow pck1 ) pck1  in
  let sbk1  = sak1 + (t8k1 .&. bk1)                                                       in  -- let !_ = traceW ("sbk1  = " <> bitShow sbk1 ) sbk1  in

  let rrr   = sbk1 + pck1 + (((x .>. fromIntegral sbk1) .&. ((pck1 .<. 1) .|. 1)) .<. 1)  in  -- let !_ = traceW ("rrr   = " <> bitShow rrr  ) rrr   in

  rrr
