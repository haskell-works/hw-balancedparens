module HaskellWorks.Data.BalancedParens.Internal.Word16
  ( mu0
  , mu1
  , mu2
  , mu3
  , mu4
  ) where

import Data.Word

-- | Subwords of size 2 ^ 0 alternating between all bits cleared and all bits
mu0 :: Word16
mu0 = 0x5555
{-# INLINE mu0 #-}

-- | Subwords of size 2 ^ 1 alternating between all bits cleared and all bits
mu1 :: Word16
mu1 = 0x3333
{-# INLINE mu1 #-}

-- | Subwords of size 2 ^ 2 alternating between all bits cleared and all bits
mu2 :: Word16
mu2 = 0x0f0f
{-# INLINE mu2 #-}

-- | Subwords of size 2 ^ 3 alternating between all bits cleared and all bits
mu3 :: Word16
mu3 = 0x00ff
{-# INLINE mu3 #-}

-- | Subwords of size 2 ^ 4 alternating between all bits cleared and all bits
mu4 :: Word16
mu4 = 0xffff
{-# INLINE mu4 #-}
