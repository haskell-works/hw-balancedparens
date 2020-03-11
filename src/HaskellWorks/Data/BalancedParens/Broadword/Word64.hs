{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word64
  ( findClose
  , findCloseFar
  ) where

import Data.Int
import Data.Semigroup                          ((<>))
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word64
import HaskellWorks.Data.Bits.Word64

muk1 :: Word64
muk1 = 0x3333333333333333
{-# INLINE muk1 #-}

muk2 :: Word64
muk2 = 0x0f0f0f0f0f0f0f0f
{-# INLINE muk2 #-}

muk3 :: Word64
muk3 = 0x00ff00ff00ff00ff
{-# INLINE muk3 #-}

muk4 :: Word64
muk4 = 0x0000ffff0000ffff
{-# INLINE muk4 #-}

muk5 :: Word64
muk5 = 0x00000000ffffffff
{-# INLINE muk5 #-}

findCloseFar :: Word64 -> Word64 -> Word64
findCloseFar p w =
  let x     = w .>. p                                                                     in
  let wsz   = 64 :: Int64                                                                 in
  let k1    = 1                                                                           in
  let k2    = 2                                                                           in
  let k3    = 3                                                                           in
  let k4    = 4                                                                           in
  let k5    = 5                                                                           in
  let k6    = 6                                                                           in
  let mask1 = (1 .<. (1 .<. k1)) - 1                                                      in
  let mask2 = (1 .<. (1 .<. k2)) - 1                                                      in
  let mask3 = (1 .<. (1 .<. k3)) - 1                                                      in
  let mask4 = (1 .<. (1 .<. k4)) - 1                                                      in
  let mask5 = (1 .<. (1 .<. k5)) - 1                                                      in
  let mask6 = (1 .<. (1 .<. k6)) - 1                                                      in
  let t64k1 = 1 .<. k1 :: Word64                                                          in
  let t64k2 = 1 .<. k2 :: Word64                                                          in
  let t64k3 = 1 .<. k3 :: Word64                                                          in
  let t64k4 = 1 .<. k4 :: Word64                                                          in
  let t64k5 = 1 .<. k5 :: Word64                                                          in
  let t8k1  = 1 .<. k1 :: Word64                                                          in
  let t8k2  = 1 .<. k2 :: Word64                                                          in
  let t8k3  = 1 .<. k3 :: Word64                                                          in
  let t8k4  = 1 .<. k4 :: Word64                                                          in
  let t8k5  = 1 .<. k5 :: Word64                                                          in
  let t8k6  = 1 .<. k6 :: Word64                                                          in

  let b0    =      x .&. 0x5555555555555555                                               in
  let b1    =    ( x .&. 0xaaaaaaaaaaaaaaaa) .>. 1                                        in
  let ll    =   (b0  .^. b1  ) .&. b1                                                     in
  let ok1   = ( (b0  .&. b1  )                         .<. 1) .|. ll                      in
  let ck1   = (((b0  .|. b1  ) .^. 0x5555555555555555) .<. 1) .|. ll                      in

  let eok1 =   ok1 .&.  muk1                                                              in
  let eck1 =  (ck1 .&. (muk1 .<. t64k1)) .>. t64k1                                        in
  let ok2L =  (ok1 .&. (muk1 .<. t64k1)) .>. t64k1                                        in
  let ok2R = kBitDiffPos 4 eok1 eck1                                                      in
  let ok2  = ok2L + ok2R                                                                  in
  let ck2  =  (ck1 .&.  muk1) + kBitDiffPos 4 eck1 eok1                                   in

  let eok2 =   ok2 .&.  muk2                                                              in
  let eck2 =  (ck2 .&. (muk2 .<. t64k2)) .>. t64k2                                        in
  let ok3L = ((ok2 .&. (muk2 .<. t64k2)) .>. t64k2)                                       in
  let ok3R = kBitDiffPos 8 eok2 eck2                                                      in
  let ok3  = ok3L + ok3R                                                                  in
  let ck3  =  (ck2 .&.  muk2) + kBitDiffPos 8 eck2 eok2                                   in

  let eok3 =   ok3 .&.  muk3                                                              in
  let eck3 =  (ck3 .&. (muk3 .<. t64k3)) .>. t64k3                                        in
  let ok4L = ((ok3 .&. (muk3 .<. t64k3)) .>. t64k3)                                       in
  let ok4R = kBitDiffPos 16 eok3 eck3                                                     in
  let ok4  = ok4L + ok4R                                                                  in
  let ck4  =  (ck3 .&.  muk3) + kBitDiffPos 16 eck3 eok3                                  in

  let eok4 =   ok4 .&.  muk4                                                              in
  let eck4 =  (ck4 .&. (muk4 .<. t64k4)) .>. t64k4                                        in
  let ok5L = ((ok4 .&. (muk4 .<. t64k4)) .>. t64k4)                                       in
  let ok5R = kBitDiffPos 32 eok4 eck4                                                     in
  let ok5  = ok5L + ok5R                                                                  in
  let ck5  =  (ck4 .&.  muk4) + kBitDiffPos 32 eck4 eok4                                  in

  let eok5 =   ok5 .&.  muk5                                                              in
  let eck5 =  (ck5 .&. (muk5 .<. t64k5)) .>. t64k5                                        in
  let ok6L = ((ok5 .&. (muk5 .<. t64k5)) .>. t64k5)                                       in
  let ok6R = kBitDiffPos 32 eok5 eck5                                                     in
  let ok6  = ok6L + ok6R                                                                  in
  let ck6  =  (ck5 .&.  muk5) + kBitDiffPos 32 eck5 eok5                                  in

  let qak6  = 0                                                                           in
  let sak6  = 0                                                                           in

  let fk6   = (ck6 .>. fromIntegral sak6) .&. mask6                                       in
  let gk6   = qak6 - fk6                                                                  in
  let bak6  = gk6 .>. fromIntegral (wsz - 1)                                              in
  let bk6   = bak6 - 1                                                                    in
  let mk6   = bk6 .&. mask6                                                               in
  let pbk6  = qak6 - ((ck6 .>. fromIntegral sak6) .&. mk6)                                in
  let pck6  = pbk6 + ((ok6 .>. fromIntegral sak6) .&. mk6)                                in
  let sbk6  = sak6 + (t8k6 .&. bk6)                                                       in

  let qak5  = pck6                                                                        in
  let sak5  = sbk6                                                                        in

  let ek5   = 0x0000002000000020 .&. comp (0xffffffffffffffff .>. fromIntegral sak5)      in
  let fk5   = ((ck5 .>. fromIntegral sak5) .|. ek5) .&. mask5                             in
  let gk5   = qak5 - fk5                                                                  in
  let bak5  = gk5 .>. fromIntegral (wsz- 1)                                               in
  let bk5   = bak5 - 1                                                                    in
  let mk5   = bk5 .&. mask5                                                               in
  let pbk5  = qak5 - ((ck5 .>. fromIntegral sak5) .|. ek5) .&. mk5                        in
  let pck5  = pbk5 +  (ok5 .>. fromIntegral sak5)          .&. mk5                        in
  let sbk5  = sak5 + (t8k5 .&. bk5)                                                       in

  let qak4  = pck5                                                                        in
  let sak4  = sbk5                                                                        in

  let ek4   = 0x0010001000100010 .&. comp (0xffffffffffffffff .>. fromIntegral sak4)      in
  let fk4   = ((ck4 .>. fromIntegral sak4) .|. ek4) .&. mask4                             in
  let gk4   = qak4 - fk4                                                                  in
  let bak4  = gk4 .>. fromIntegral (wsz - 1)                                              in
  let bk4   = bak4 - 1                                                                    in
  let mk4   = bk4 .&. mask4                                                               in
  let pbk4  = qak4 - ((ck4 .>. fromIntegral sak4) .|. ek4) .&. mk4                        in
  let pck4  = pbk4 +  (ok4 .>. fromIntegral sak4)          .&. mk4                        in
  let sbk4  = sak4 + (t8k4 .&. bk4)                                                       in

  let qak3  = pck4                                                                        in
  let sak3  = sbk4                                                                        in

  let ek3   = 0x0808080808080808 .&. comp (0xffffffffffffffff .>. fromIntegral sak3)      in
  let fk3   = ((ck3 .>. fromIntegral sak3) .|. ek3) .&. mask3                             in
  let gk3   = qak3 - fk3                                                                  in
  let bak3  = gk3 .>. fromIntegral (wsz - 1)                                              in
  let bk3   = bak3 - 1                                                                    in
  let mk3   = bk3 .&. mask3                                                               in
  let pbk3  = qak3 - ((ck3 .>. fromIntegral sak3) .|. ek3) .&. mk3                        in
  let pck3  = pbk3 +  (ok3 .>. fromIntegral sak3)          .&. mk3                        in
  let sbk3  = sak3 + (t8k3 .&. bk3)                                                       in

  let qak2  = pck3                                                                        in
  let sak2  = sbk3                                                                        in

  let ek2   = 0xaaaaaaaaaaaaaaaa .&. comp (0xffffffffffffffff .>. fromIntegral sak2)      in
  let fk2   = ((ck2 .>. fromIntegral sak2) .|. ek2) .&. mask2                             in
  let gk2   = qak2 - fk2                                                                  in
  let bak2  = gk2 .>. fromIntegral (wsz - 1)                                              in
  let bk2   = bak2 - 1                                                                    in
  let mk2   = bk2 .&. mask2                                                               in
  let pbk2  = qak2 - ((ck2 .>. fromIntegral sak2) .|. ek2) .&. mk2                        in
  let pck2  = pbk2 +  (ok2 .>. fromIntegral sak2)          .&. mk2                        in
  let sbk2  = sak2 + (t8k2 .&. bk2)                                                       in

  let qak1  = pck2                                                                        in
  let sak1  = sbk2                                                                        in

  let ek1   = 0xaaaaaaaaaaaaaaaa .&. comp (0xffffffffffffffff .>. fromIntegral sak1)      in
  let fk1   = ((ck1 .>. fromIntegral sak1) .|. ek1) .&. mask1                             in
  let gk1   = qak1 - fk1                                                                  in
  let bk1   = (gk1 .>. fromIntegral (wsz - 1)) - 1                                        in
  let mk1   = bk1  .&. mask1                                                              in
  let pc1   = ((ck1 .>. fromIntegral sak1) .|. ek1) .&. mk1                               in
  let po1   =  (ok1 .>. fromIntegral sak1)          .&. mk1                               in
  let pbk1  = qak1 - pc1                                                                  in
  let pck1  = pbk1 + po1                                                                  in
  let sbk1  = sak1 + (t8k1 .&. bk1)                                                       in

  let rrr   = sbk1 + pck1 + (((x .>. fromIntegral sbk1) .&. ((pck1 .<. 1) .|. 1)) .<. 1)  in

  rrr + p
{-# INLINE findCloseFar #-}

-- Source:
--    Broadword Implementation of Parenthesis Queries
--    Sebastiano Vigna
--    Dipartimento di Scienze dell’Informazione
--    Università degli Studi di Milano, Italy
findClose :: Word64 -> Word64
findClose x =
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
{-# INLINE findClose #-}
