{-# LANGUAGE LambdaCase #-}

module HaskellWorks.Data.BalancedParens.Internal.Slow.FindCloseN.Generic
  ( findCloseN
  ) where

import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

findCloseN :: (BitLength a, CloseAt a, TestBit a) => a -> Count -> Count -> Maybe Count
findCloseN v c p = if 0 < p
  then if v `closeAt` p
    then if c <= 1
      then Just p
      else findCloseN v (c - 1) (p + 1)
    else findCloseN v (c + 1) (p + 1)
  else Nothing
{-# INLINE findCloseN #-}
