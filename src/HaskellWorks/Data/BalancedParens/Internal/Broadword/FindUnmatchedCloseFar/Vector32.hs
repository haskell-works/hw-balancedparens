module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector32
  ( findUnmatchedCloseFar
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Int.Unsigned
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable                                                             as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word32 as BWW32
import qualified HaskellWorks.Data.Drop                                                           as HW
import qualified HaskellWorks.Data.Length                                                         as HW

findUnmatchedCloseCont :: Int64 -> Count -> DVS.Vector Word32 -> Count
findUnmatchedCloseCont i c v = if i < HW.end v
  then case BWW32.findUnmatchedCloseFar c 0 w of
    q -> if q >= bitLength w
      then findUnmatchedCloseCont (i + 1) (q - bitLength w) v
      else b + q
  else b + c
  where b  = unsigned i * bitLength w -- base
        w  = v !!! fromIntegral i
{-# INLINE findUnmatchedCloseCont #-}

findUnmatchedClose' :: Word64 -> Word64 -> DVS.Vector Word32 -> Count
findUnmatchedClose' c p v = if DVS.length v > 0
    then case BWW32.findUnmatchedCloseFar c p w of
        q -> if q >= bitLength w
          then findUnmatchedCloseCont 1 (q - bitLength w) v
          else q
    else p * 2 + c
  where w  = v !!! 0
{-# INLINE findUnmatchedClose' #-}

findUnmatchedCloseFar :: Word64 -> Word64 -> DVS.Vector Word32 -> Count
findUnmatchedCloseFar c p v = findUnmatchedClose' c (p - vd) (HW.drop vi v) + vd
  where vi = p `div` elemBitLength v
        vd = vi * elemBitLength v
{-# INLINE findUnmatchedCloseFar #-}
