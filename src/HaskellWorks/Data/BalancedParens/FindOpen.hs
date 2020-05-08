{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.BalancedParens.FindOpen
  ( FindOpen(..)
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.FindOpenN
import HaskellWorks.Data.BalancedParens.OpenAt
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable as DVS

class FindOpen v where
  findOpen :: v -> Count -> Maybe Count

instance (FindOpen a) => FindOpen (BitShown a) where
  findOpen = findOpen . bitShown
  {-# INLINE findOpen #-}

instance FindOpen [Bool] where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen (DVS.Vector Word8) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen (DVS.Vector Word16) where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen (DVS.Vector Word32) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen (DVS.Vector Word64) where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen Word8 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen Word16 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen Word32 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen Word64 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance FindOpen (Naive Word64) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}
