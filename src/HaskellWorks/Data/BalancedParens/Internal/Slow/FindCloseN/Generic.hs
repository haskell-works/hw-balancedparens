module HaskellWorks.Data.BalancedParens.Internal.Slow.FindCloseN.Generic
  ( findCloseN
  ) where

import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

-- | Find the position of the corresponding close parenthesis carrying in a number of open parentheses starting from a given position.
--
-- All positions are one based.
--
-- This is a reference implementation.
--
-- >>> :set -XTypeApplications
-- >>> import HaskellWorks.Data.Bits.BitRead
-- >>> import Data.Maybe
-- >>> import Data.Word
-- >>> findCloseN (fromJust (bitRead @Word64 "1100000")) 0 1
-- Just 4
-- >>> findCloseN (fromJust (bitRead @Word64 "1100000")) 0 2
-- Just 3
-- >>> findCloseN (fromJust (bitRead @Word64 "1100000")) 1 1
-- Just 5
-- >>> findCloseN (fromJust (bitRead @Word64 "1100000")) 1 2
-- Just 4
findCloseN :: (BitLength a, CloseAt a, TestBit a) => a -> Count -> Count -> Maybe Count
findCloseN v c p = if 0 < p
  then if v `closeAt` p
    then if c <= 1
      then Just p
      else findCloseN v (c - 1) (p + 1)
    else findCloseN v (c + 1) (p + 1)
  else Nothing
{-# INLINE findCloseN #-}
