module HaskellWorks.Data.BalancedParens.Internal.Word8
  ( mu0
  , mu1
  , mu2
  , mu3
  ) where

import Data.Word

-- | Subwords of size 2 ^ 0 alternating between all bits cleared and all bits
mu0 :: Word16
mu0 = 0x55
{-# INLINE mu0 #-}

-- | Subwords of size 2 ^ 1 alternating between all bits cleared and all bits
mu1 :: Word16
mu1 = 0x33
{-# INLINE mu1 #-}

-- | Subwords of size 2 ^ 2 alternating between all bits cleared and all bits
mu2 :: Word16
mu2 = 0x0f
{-# INLINE mu2 #-}

-- | Subwords of size 2 ^ 3 alternating between all bits cleared and all bits
mu3 :: Word16
mu3 = 0xff
{-# INLINE mu3 #-}
