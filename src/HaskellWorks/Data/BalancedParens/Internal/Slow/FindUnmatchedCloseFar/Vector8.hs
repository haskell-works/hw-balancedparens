
module HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Vector8
  ( findUnmatchedCloseFar
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Int.Signed

import qualified Data.Vector.Storable as DVS

-- | Find the position of the first unmatch parenthesis.
--
-- The digits 1 and 0 are treated as an open parenthesis and closing parenthesis respectively.
--
-- All positions are indexed from zero.  If the search runs out of bits, then continue as if there remain an infinite
-- string of zeros.
--
-- >>> import HaskellWorks.Data.Bits.BitRead
-- >>> import Data.Maybe
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string:
--
-- >>> findUnmatchedCloseFar 0 0 $ fromJust $ bitRead "00000000"
-- 0
--
-- The following scans for the first unmatched closing parenthesis after skipping one bit from the beginning of the
-- bit string:
--
-- >>> findUnmatchedCloseFar 0 1 $ fromJust $ bitRead "00000000"
-- 1
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string.  To find
-- unmatched parenthesis, the scan passes over the first parent of matching parentheses:
--
-- >>> findUnmatchedCloseFar 0 0 $ fromJust $ bitRead "10000000"
-- 2
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string, but runs
-- out of bits.  The scan continues as if there an inifinite string of zero bits follows, the first of which is at
-- position 64, which also happens to be the position of the unmatched parenthesis.
--
-- >>> findUnmatchedCloseFar 0 0 $ fromJust $ bitRead "11110000"
-- 8
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string, but runs
-- out of bits.  The scan continues as if there an inifinite string of zero bits follows and we don't get to the
-- unmatched parenthesis until position 128.
--
-- >>> findUnmatchedCloseFar 0 0 $ fromJust $ bitRead "11111111"
-- 16
--
-- Following are some more examples:
--
-- >>> findUnmatchedCloseFar 0 0 $ fromJust $ bitRead "11110000"
-- 8
-- >>> findUnmatchedCloseFar 0 1 $ fromJust $ bitRead "11110000"
-- 7
-- >>> findUnmatchedCloseFar 0 2 $ fromJust $ bitRead "11110000"
-- 6
-- >>> findUnmatchedCloseFar 0 3 $ fromJust $ bitRead "11110000"
-- 5
-- >>> findUnmatchedCloseFar 0 4 $ fromJust $ bitRead "11110000"
-- 4
-- >>> findUnmatchedCloseFar 0 5 $ fromJust $ bitRead "11110000"
-- 5
-- >>> findUnmatchedCloseFar 0 6 $ fromJust $ bitRead "11110000"
-- 6
-- >>> findUnmatchedCloseFar 0 7 $ fromJust $ bitRead "11110000"
-- 7
-- >>> findUnmatchedCloseFar 0 8 $ fromJust $ bitRead "11110000"
-- 8
findUnmatchedCloseFar :: Word64 -> Word64 -> DVS.Vector Word8 -> Word64
findUnmatchedCloseFar d i v = if i < bitLength v
  then if v .?. signed i
    then findUnmatchedCloseFar (d + 1) (i + 1) v
    else if d == 0
      then i
      else findUnmatchedCloseFar (d - 1) (i + 1) v
  else bitLength v + d
{-# INLINE findUnmatchedCloseFar #-}
