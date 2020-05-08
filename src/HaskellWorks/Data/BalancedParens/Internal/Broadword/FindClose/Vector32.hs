module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector32
  ( findClose
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Int.Unsigned
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable                                                             as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word32 as BW32
import qualified HaskellWorks.Data.Drop                                                           as HW
import qualified HaskellWorks.Data.Length                                                         as HW

findCloseCont :: DVS.Vector Word32 -> Int64 -> Count -> Maybe Count
findCloseCont v i c = if i < HW.end v
  then case BW32.findUnmatchedCloseFar c 0 w of
    q -> if q >= bitLength w
      then findCloseCont v (i + 1) (q - bitLength w)
      else Just (b + q + 1)
  else Just (b + c + 1)
  where b  = unsigned i * bitLength w -- base
        w  = v !!! fromIntegral i
{-# INLINE findCloseCont #-}

findClose :: DVS.Vector Word32 -> Count -> Maybe Count
findClose _ 0 = Nothing
findClose v p = fmap (+ vd) (findClose' (HW.drop vi v) (p - vd))
  where vi = (p - 1) `div` elemBitLength v
        vd = vi * elemBitLength v
{-# INLINE findClose #-}

findClose' :: DVS.Vector Word32 -> Count -> Maybe Count
findClose' v p = if DVS.length v > 0
    then if closeAt w p
      then Just p
      else case BW32.findUnmatchedCloseFar 0 p w of
        q -> if q >= bitLength w
          then  findCloseCont v 1 (q - bitLength w)
          else Just (q + 1)
    else Just (p * 2)
  where w  = v !!! 0
{-# INLINE findClose' #-}
