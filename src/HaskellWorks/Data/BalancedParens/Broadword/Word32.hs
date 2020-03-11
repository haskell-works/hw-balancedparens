{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word32
  ( findCloseFar
  ) where

import Data.Int
import Data.Semigroup                          ((<>))
import Data.Word
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word32

traceW :: Show a => String -> a -> a
traceW _ a = a
-- traceW s a = trace (s <> " = " <> show a) a

muk1 :: Word32
muk1 = 0x33333333
{-# INLINE muk1 #-}

muk2 :: Word32
muk2 = 0x0f0f0f0f
{-# INLINE muk2 #-}

muk3 :: Word32
muk3 = 0x00ff00ff
{-# INLINE muk3 #-}

muk4 :: Word32
muk4 = 0x0000ffff
{-# INLINE muk4 #-}

findCloseFar :: Word32 -> Word32 -> Word32
findCloseFar p x =                                                                            let !_ = traceW ("findCloseFar " <> show p <> " " <> bitShow x) (p, x) in
  let w     = 32 :: Int32                                                                 in  let !_ = traceW ("w     = " <> bitShow w    ) w     in
  let k1    = 1                                                                           in  let !_ = traceW ("k1    = " <> bitShow k1   ) k1    in
  let k2    = 2                                                                           in  let !_ = traceW ("k2    = " <> bitShow k2   ) k2    in
  let k3    = 3                                                                           in  let !_ = traceW ("k3    = " <> bitShow k3   ) k3    in
  let k4    = 4                                                                           in  let !_ = traceW ("k4    = " <> bitShow k4   ) k4    in
  let k5    = 5                                                                           in  let !_ = traceW ("k5    = " <> bitShow k5   ) k5    in
  let mask1 = (1 .<. (1 .<. k1)) - 1                                                      in  let !_ = traceW ("mask1 = " <> bitShow mask1) mask1 in
  let mask2 = (1 .<. (1 .<. k2)) - 1                                                      in  let !_ = traceW ("mask2 = " <> bitShow mask2) mask2 in
  let mask3 = (1 .<. (1 .<. k3)) - 1                                                      in  let !_ = traceW ("mask3 = " <> bitShow mask3) mask3 in
  let mask4 = (1 .<. (1 .<. k4)) - 1                                                      in  let !_ = traceW ("mask4 = " <> bitShow mask4) mask4 in
  let mask5 = (1 .<. (1 .<. k5)) - 1                                                      in  let !_ = traceW ("mask5 = " <> bitShow mask5) mask5 in
  let t64k1 = 1 .<. k1 :: Word64                                                          in  let !_ = traceW ("t64k1 = " <> bitShow t64k1) t64k1 in
  let t64k2 = 1 .<. k2 :: Word64                                                          in  let !_ = traceW ("t64k2 = " <> bitShow t64k2) t64k2 in
  let t64k3 = 1 .<. k3 :: Word64                                                          in  let !_ = traceW ("t64k3 = " <> bitShow t64k3) t64k3 in
  let t64k4 = 1 .<. k4 :: Word64                                                          in  let !_ = traceW ("t64k4 = " <> bitShow t64k4) t64k4 in
  let t8k1  = 1 .<. k1 :: Word32                                                          in  let !_ = traceW ("t8k1  = " <> bitShow t8k1 ) t8k1  in
  let t8k2  = 1 .<. k2 :: Word32                                                          in  let !_ = traceW ("t8k2  = " <> bitShow t8k2 ) t8k2  in
  let t8k3  = 1 .<. k3 :: Word32                                                          in  let !_ = traceW ("t8k3  = " <> bitShow t8k3 ) t8k3  in
  let t8k4  = 1 .<. k4 :: Word32                                                          in  let !_ = traceW ("t8k4  = " <> bitShow t8k4 ) t8k4  in
  let t8k5  = 1 .<. k5 :: Word32                                                          in  let !_ = traceW ("t8k5  = " <> bitShow t8k5 ) t8k5  in

  let b0    =      x .&. 0x55555555                                                       in  let !_ = traceW ("b0    = " <> bitShow b0   ) b0    in
  let b1    =    ( x .&. 0xaaaaaaaa) .>. 1                                                in  let !_ = traceW ("b1    = " <> bitShow b1   ) b1    in
  let ll    =   (b0  .^. b1  ) .&. b1                                                     in  let !_ = traceW ("ll    = " <> bitShow ll   ) ll    in
  let ok1   = ( (b0  .&. b1  )                 .<. 1) .|. ll                              in  let !_ = traceW ("ok1   = " <> bitShow ok1  ) ok1   in
  let ck1   = (((b0  .|. b1  ) .^. 0x55555555) .<. 1) .|. ll                              in  let !_ = traceW ("ck1   = " <> bitShow ck1  ) ck1   in

  let eok1 =   ok1 .&.  muk1                                                              in  let !_ = traceW ("eok1  = " <> bitShow eok1 ) eok1  in
  let eck1 =  (ck1 .&. (muk1 .<. t64k1)) .>. t64k1                                        in  let !_ = traceW ("eck1  = " <> bitShow eck1 ) eck1  in
  let ok2L =  (ok1 .&. (muk1 .<. t64k1)) .>. t64k1                                        in  let !_ = traceW ("ok2L  = " <> bitShow ok2L ) ok2L  in
  let ok2R = kBitDiffPos 4 eok1 eck1                                                      in  let !_ = traceW ("ok2R  = " <> bitShow ok2R ) ok2R  in
  let ok2  = ok2L + ok2R                                                                  in  let !_ = traceW ("ok2   = " <> bitShow ok2  ) ok2   in
  let ck2  =  (ck1 .&.  muk1) + kBitDiffPos 4 eck1 eok1                                   in  let !_ = traceW ("ck2   = " <> bitShow ck2  ) ck2   in

  let eok2 =   ok2 .&.  muk2                                                              in  let !_ = traceW ("eok2  = " <> bitShow eok2 ) eok2  in
  let eck2 =  (ck2 .&. (muk2 .<. t64k2)) .>. t64k2                                        in  let !_ = traceW ("eck2  = " <> bitShow eck2 ) eck2  in
  let ok3L = ((ok2 .&. (muk2 .<. t64k2)) .>. t64k2)                                       in  let !_ = traceW ("ok3L  = " <> bitShow ok3L ) ok3L  in
  let ok3R = kBitDiffPos 8 eok2 eck2                                                      in  let !_ = traceW ("ok3R  = " <> bitShow ok3R ) ok3R  in
  let ok3  = ok3L + ok3R                                                                  in  let !_ = traceW ("ok3   = " <> bitShow ok3  ) ok3   in
  let ck3  =  (ck2 .&.  muk2) + kBitDiffPos 8 eck2 eok2                                   in  let !_ = traceW ("ck3   = " <> bitShow ck3  ) ck3   in

  let eok3 =   ok3 .&.  muk3                                                              in  let !_ = traceW ("eok3  = " <> bitShow eok3 ) eok3  in
  let eck3 =  (ck3 .&. (muk3 .<. t64k3)) .>. t64k3                                        in  let !_ = traceW ("eck3  = " <> bitShow eck3 ) eck3  in
  let ok4L = ((ok3 .&. (muk3 .<. t64k3)) .>. t64k3)                                       in  let !_ = traceW ("ok4L  = " <> bitShow ok4L ) ok4L  in
  let ok4R = kBitDiffPos 16 eok3 eck3                                                     in  let !_ = traceW ("ok4R  = " <> bitShow ok4R ) ok4R  in
  let ok4  = ok4L + ok4R                                                                  in  let !_ = traceW ("ok4   = " <> bitShow ok4  ) ok4   in
  let ck4  =  (ck3 .&.  muk3) + kBitDiffPos 16 eck3 eok3                                  in  let !_ = traceW ("ck4   = " <> bitShow ck4  ) ck4   in

  let eok4 =   ok4 .&.  muk4                                                              in  let !_ = traceW ("eok4  = " <> bitShow eok4 ) eok4  in
  let eck4 =  (ck4 .&. (muk4 .<. t64k4)) .>. t64k4                                        in  let !_ = traceW ("eck4  = " <> bitShow eck4 ) eck4  in
  let ok5L = ((ok4 .&. (muk4 .<. t64k4)) .>. t64k4)                                       in  let !_ = traceW ("ok5L  = " <> bitShow ok5L ) ok5L  in
  let ok5R = kBitDiffPos 32 eok4 eck4                                                     in  let !_ = traceW ("ok5R  = " <> bitShow ok5R ) ok5R  in
  let ok5  = ok5L + ok5R                                                                  in  let !_ = traceW ("ok5   = " <> bitShow ok5  ) ok5   in
  let ck5  =  (ck4 .&.  muk4) + kBitDiffPos 32 eck4 eok4                                  in  let !_ = traceW ("ck5   = " <> bitShow ck5  ) ck5   in

  let pak5  = p                                                                           in  let !_ = traceW ("pak5  = " <> bitShow pak5 ) pak5  in
  let sak5  = 0                                                                           in  let !_ = traceW ("sak5  = " <> bitShow sak5 ) sak5  in

  let fk5   = (ck5 .>. fromIntegral sak5) .&. mask5                                       in  let !_ = traceW ("fk5   = " <> bitShow fk5  ) fk5   in
  let gk5   = pak5 - fk5                                                                  in  let !_ = traceW ("gk5   = " <> bitShow gk5  ) gk5   in
  let bak5  = gk5 .>. fromIntegral (w - 1)                                                in  let !_ = traceW ("bak5  = " <> bitShow bak5 ) bak5  in
  let bk5   = bak5 - 1                                                                    in  let !_ = traceW ("bk5   = " <> bitShow bk5  ) bk5   in
  let mk5   = bk5 .&. mask5                                                               in  let !_ = traceW ("mk5   = " <> bitShow mk5  ) mk5   in
  let pbk5  = pak5 - ((ck5 .>. fromIntegral sak5) .&. mk5)                                in  let !_ = traceW ("pbk5  = " <> bitShow pbk5 ) pbk5  in
  let pck5  = pbk5 + ((ok5 .>. fromIntegral sak5) .&. mk5)                                in  let !_ = traceW ("pck5  = " <> bitShow pck5 ) pck5  in
  let sbk5  = sak5 + (t8k5 .&. bk5)                                                       in  let !_ = traceW ("sbk5  = " <> bitShow sbk5 ) sbk5  in

  let pak4  = pck5                                                                        in  let !_ = traceW ("pak4  = " <> bitShow pak4 ) pak4  in
  let sak4  = sbk5                                                                        in  let !_ = traceW ("sak4  = " <> bitShow sak4 ) sak4  in

  let ek4   = 0x00100010 .&. comp (0xffffffff .>. fromIntegral sak4)                      in  let !_ = traceW ("ek4   = " <> bitShow ek4  ) ek4   in
  let fk4   = ((ck4 .>. fromIntegral sak4) .|. ek4) .&. mask4                             in  let !_ = traceW ("fk4   = " <> bitShow fk4  ) fk4   in
  let gk4   = pak4 - fk4                                                                  in  let !_ = traceW ("gk4   = " <> bitShow gk4  ) gk4   in
  let bak4  = gk4 .>. fromIntegral (w - 1)                                                in  let !_ = traceW ("bak4  = " <> bitShow bak4 ) bak4  in
  let bk4   = bak4 - 1                                                                    in  let !_ = traceW ("bk4   = " <> bitShow bk4  ) bk4   in
  let mk4   = bk4 .&. mask4                                                               in  let !_ = traceW ("mk4   = " <> bitShow mk4  ) mk4   in
  let pbk4  = pak4 - ((ck4 .>. fromIntegral sak4) .|. ek4) .&. mk4                        in  let !_ = traceW ("pbk4  = " <> bitShow pbk4 ) pbk4  in
  let pck4  = pbk4 +  (ok4 .>. fromIntegral sak4)          .&. mk4                        in  let !_ = traceW ("pck4  = " <> bitShow pck4 ) pck4  in
  let sbk4  = sak4 + (t8k4 .&. bk4)                                                       in  let !_ = traceW ("sbk4  = " <> bitShow sbk4 ) sbk4  in

  let pak3  = pck4                                                                        in  let !_ = traceW ("pak3  = " <> bitShow pak3 ) pak3  in
  let sak3  = sbk4                                                                        in  let !_ = traceW ("sak3  = " <> bitShow sak3 ) sak3  in

  let ek3   = 0x08080808 .&. comp (0xffffffff .>. fromIntegral sak3)                      in  let !_ = traceW ("ek3   = " <> bitShow ek3  ) ek3   in
  let fk3   = ((ck3 .>. fromIntegral sak3) .|. ek3) .&. mask3                             in  let !_ = traceW ("fk3   = " <> bitShow fk3  ) fk3   in
  let gk3   = pak3 - fk3                                                                  in  let !_ = traceW ("gk3   = " <> bitShow gk3  ) gk3   in
  let bak3  = gk3 .>. fromIntegral (w - 1)                                                in  let !_ = traceW ("bak3  = " <> bitShow bak3 ) bak3  in
  let bk3   = bak3 - 1                                                                    in  let !_ = traceW ("bk3   = " <> bitShow bk3  ) bk3   in
  let mk3   = bk3 .&. mask3                                                               in  let !_ = traceW ("mk3   = " <> bitShow mk3  ) mk3   in
  let pbk3  = pak3 - ((ck3 .>. fromIntegral sak3) .|. ek3) .&. mk3                        in  let !_ = traceW ("pbk3  = " <> bitShow pbk3 ) pbk3  in
  let pck3  = pbk3 +  (ok3 .>. fromIntegral sak3)          .&. mk3                        in  let !_ = traceW ("pck3  = " <> bitShow pck3 ) pck3  in
  let sbk3  = sak3 + (t8k3 .&. bk3)                                                       in  let !_ = traceW ("sbk3  = " <> bitShow sbk3 ) sbk3  in

  let pak2  = pck3                                                                        in  let !_ = traceW ("pak2  = " <> bitShow pak2 ) pak2  in
  let sak2  = sbk3                                                                        in  let !_ = traceW ("sak2  = " <> bitShow sak2 ) sak2  in

  let ek2   = 0xaaaaaaaa .&. comp (0xffffffff .>. fromIntegral sak2)                      in  let !_ = traceW ("ek2   = " <> bitShow ek2  ) ek2   in
  let fk2   = ((ck2 .>. fromIntegral sak2) .|. ek2) .&. mask2                             in  let !_ = traceW ("fk2   = " <> bitShow fk2  ) fk2   in
  let gk2   = pak2 - fk2                                                                  in  let !_ = traceW ("gk2   = " <> bitShow gk2  ) gk2   in
  let bak2  = gk2 .>. fromIntegral (w - 1)                                                in  let !_ = traceW ("bak2  = " <> bitShow bak2 ) bak2  in
  let bk2   = bak2 - 1                                                                    in  let !_ = traceW ("bk2   = " <> bitShow bk2  ) bk2   in
  let mk2   = bk2 .&. mask2                                                               in  let !_ = traceW ("mk2   = " <> bitShow mk2  ) mk2   in
  let pbk2  = pak2 - ((ck2 .>. fromIntegral sak2) .|. ek2) .&. mk2                        in  let !_ = traceW ("pbk2  = " <> bitShow pbk2 ) pbk2  in
  let pck2  = pbk2 +  (ok2 .>. fromIntegral sak2)          .&. mk2                        in  let !_ = traceW ("pck2  = " <> bitShow pck2 ) pck2  in
  let sbk2  = sak2 + (t8k2 .&. bk2)                                                       in  let !_ = traceW ("sbk2  = " <> bitShow sbk2 ) sbk2  in

  let pak1  = pck2                                                                        in  let !_ = traceW ("pak1  = " <> bitShow pak1 ) pak1  in
  let sak1  = sbk2                                                                        in  let !_ = traceW ("sak1  = " <> bitShow sak1 ) sak1  in

  let ek1   = 0xaaaaaaaa .&. comp (0xffffffff .>. fromIntegral sak1)                      in  let !_ = traceW ("ek1   = " <> bitShow ek1  ) ek1   in
  let fk1   = ((ck1 .>. fromIntegral sak1) .|. ek1) .&. mask1                             in  let !_ = traceW ("fk1   = " <> bitShow fk1  ) fk1   in
  let gk1   = pak1 - fk1                                                                  in  let !_ = traceW ("gk1   = " <> bitShow gk1  ) gk1   in
  let bk1   = (gk1 .>. fromIntegral (w - 1)) - 1                                          in  let !_ = traceW ("bk1   = " <> bitShow bk1  ) bk1   in
  let mk1   = bk1  .&. mask1                                                              in  let !_ = traceW ("mk1   = " <> bitShow mk1  ) mk1   in
  let pc1   = ((ck1 .>. fromIntegral sak1) .|. ek1) .&. mk1                               in  let !_ = traceW ("pc1   = " <> bitShow pc1  ) pc1   in
  let po1   =  (ok1 .>. fromIntegral sak1)          .&. mk1                               in  let !_ = traceW ("po1   = " <> bitShow po1  ) po1   in
  let pbk1  = pak1 - pc1                                                                  in  let !_ = traceW ("pbk1  = " <> bitShow pbk1 ) pbk1  in
  let pck1  = pbk1 + po1                                                                  in  let !_ = traceW ("pck1  = " <> bitShow pck1 ) pck1  in
  let sbk1  = sak1 + (t8k1 .&. bk1)                                                       in  let !_ = traceW ("sbk1  = " <> bitShow sbk1 ) sbk1  in

  let rrr   = sbk1 + pck1 + (((x .>. fromIntegral sbk1) .&. ((pck1 .<. 1) .|. 1)) .<. 1)  in  let !_ = traceW ("rrr   = " <> bitShow rrr  ) rrr   in

  rrr
