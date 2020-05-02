{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.BalancedParens.FindClose
  ( FindClose(..)
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.BalancedParens.FindCloseN
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Type
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable                                                   as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector16 as BWV16
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector32 as BWV32
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector64 as BWV64
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector8  as BWV8
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.Word64             as W64

class FindClose v where
  -- | Find the closing parenthesis that machines the open parenthesis at the current position.
  --
  -- If the parenthesis at the current position is an close parenthesis, then return the current position.
  --
  -- Indexes are 1-based.  1 corresponds to open and 0 corresponds to close.
  --
  -- If we run out of bits in the supplied bit-string, the implementation my either return Nothing, or
  -- assume all the bits that follow are zeros.
  --
  -- >>> :set -XTypeApplications
  -- >>> import Data.Maybe
  -- >>> import HaskellWorks.Data.Bits.BitRead
  -- >>> findClose (fromJust (bitRead @Word64 "00000000")) 1
  -- Just 1
  -- >>> findClose (fromJust (bitRead @Word64 "10101010")) 1
  -- Just 2
  -- >>> findClose (fromJust (bitRead @Word64 "10101010")) 2
  -- Just 2
  -- >>> findClose (fromJust (bitRead @Word64 "10101010")) 3
  -- Just 4
  -- >>> findClose (fromJust (bitRead @Word64 "11010010")) 1
  -- Just 6
  -- >>> findClose (fromJust (bitRead @Word64 "11110000")) 1
  -- Just 8
  findClose :: v -> Count -> Maybe Count

instance (FindClose a) => FindClose (BitShown a) where
  findClose = findClose . bitShown
  {-# INLINE findClose #-}

instance FindClose [Bool] where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word8) where
  findClose = BWV8.findClose
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word16) where
  findClose = BWV16.findClose
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word32) where
  findClose = BWV32.findClose
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word64) where
  findClose = BWV64.findClose
  {-# INLINE findClose #-}

instance FindClose (Naive (DVS.Vector Word8)) where
  findClose (Naive v) p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose (Naive (DVS.Vector Word16)) where
  findClose (Naive v) p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose (Naive (DVS.Vector Word32)) where
  findClose (Naive v) p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose (Naive (DVS.Vector Word64)) where
  findClose (Naive v) p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word8 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word16 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word32 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word64 where
  findClose = findClose . Broadword
  {-# INLINE findClose #-}

instance FindClose (Naive Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindClose (Broadword Word64) where
  findClose (Broadword w) p = let x = w .>. (p - 1) in
    case negate (x .&. 1) .&. W64.findUnmatchedClose x of
      127 -> Nothing
      r   -> let r' = fromIntegral r + p in if r' > 64 then Nothing else Just r'
  {-# INLINE findClose #-}
