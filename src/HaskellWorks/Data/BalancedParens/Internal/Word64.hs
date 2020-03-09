module HaskellWorks.Data.BalancedParens.Internal.Word64
  ( mu0
  , mu1
  , mu2
  , mu3
  , mu4
  , mu5
  , mu6
  ) where

import Data.Word

-- | Subwords of size 2 ^ 0 alternating between all bits cleared and all bits
mu0 :: Word64
mu0 = 0x5555555555555555
{-# INLINE mu0 #-}

-- | Subwords of size 2 ^ 1 alternating between all bits cleared and all bits
mu1 :: Word64
mu1 = 0x3333333333333333
{-# INLINE mu1 #-}

-- | Subwords of size 2 ^ 2 alternating between all bits cleared and all bits
mu2 :: Word64
mu2 = 0x0f0f0f0f0f0f0f0f
{-# INLINE mu2 #-}

-- | Subwords of size 2 ^ 3 alternating between all bits cleared and all bits
mu3 :: Word64
mu3 = 0x00ff00ff00ff00ff
{-# INLINE mu3 #-}

-- | Subwords of size 2 ^ 4 alternating between all bits cleared and all bits
mu4 :: Word64
mu4 = 0x0000ffff0000ffff
{-# INLINE mu4 #-}

-- | Subwords of size 2 ^ 5 alternating between all bits cleared and all bits
mu5 :: Word64
mu5 = 0x00000000ffffffff
{-# INLINE mu5 #-}

-- | Subwords of size 2 ^ 6 alternating between all bits cleared and all bits
mu6 :: Word64
mu6 = 0xffffffffffffffff
{-# INLINE mu6 #-}
