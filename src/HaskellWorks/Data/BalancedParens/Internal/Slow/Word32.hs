{-# LANGUAGE LambdaCase #-}

module HaskellWorks.Data.BalancedParens.Internal.Slow.Word32
  ( findUnmatchedCloseFar
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

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
-- >>> findUnmatchedCloseFar 0 $ fromJust $ bitRead "00000000 00000000 00000000 00000000"
-- 0
--
-- The following scans for the first unmatched closing parenthesis after skipping one bit from the beginning of the
-- bit string:
--
-- >>> findUnmatchedCloseFar 1 $ fromJust $ bitRead "00000000 00000000 00000000 00000000"
-- 1
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string.  To find
-- unmatched parenthesis, the scan passes over the first parent of matching parentheses:
--
-- >>> findUnmatchedCloseFar 0 $ fromJust $ bitRead "10000000 00000000 00000000 00000000"
-- 2
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string, but runs
-- out of bits.  The scan continues as if there an inifinite string of zero bits follows, the first of which is at
-- position 64, which also happens to be the position of the unmatched parenthesis.
--
-- >>> findUnmatchedCloseFar 0 $ fromJust $ bitRead "11111111 11111111 00000000 00000000"
-- 32
--
-- The following scans for the first unmatched closing parenthesis from the beginning of the bit string, but runs
-- out of bits.  The scan continues as if there an inifinite string of zero bits follows and we don't get to the
-- unmatched parenthesis until position 128.
--
-- >>> findUnmatchedCloseFar 0 $ fromJust $ bitRead "11111111 11111111 11111111 11111111"
-- 64
--
-- Following are some more examples:
--
-- >>> findUnmatchedCloseFar 0 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 32
-- >>> findUnmatchedCloseFar 1 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 7
-- >>> findUnmatchedCloseFar 2 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 6
-- >>> findUnmatchedCloseFar 3 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 5
-- >>> findUnmatchedCloseFar 4 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 4
-- >>> findUnmatchedCloseFar 5 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 5
-- >>> findUnmatchedCloseFar 6 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 6
-- >>> findUnmatchedCloseFar 7 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 7
-- >>> findUnmatchedCloseFar 8 $ fromJust $ bitRead "11110000 11110000 11110000 11110000"
-- 32
findUnmatchedCloseFar :: Word64 -> Word32 -> Word64
findUnmatchedCloseFar = go 0
  where go :: Word64 -> Word64 -> Word32 -> Word64
        go d 32 _ = 32 + d
        go d i w = case (w .>. i) .&. 1 of
          1 -> go (d + 1) (i + 1) w
          _ -> if d == 0
            then i
            else go (d - 1) (i + 1) w
