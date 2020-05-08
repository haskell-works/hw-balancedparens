{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.BalancedParens.CloseAt
  ( CloseAt(..)
  ) where

import Data.Vector.Storable                  as DVS
import Data.Word
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Type
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning

closeAt' :: (TestBit a, BitLength a) => a -> Count -> Bool
closeAt' v c = c > 0 && not (v .?. toPosition (c - 1)) || c > bitLength v
{-# INLINE closeAt' #-}

class CloseAt v where
  -- | Determine if the parenthesis at the give position (one-based) is a close.
  --
  -- >>> :set -XTypeApplications
  -- >>> import HaskellWorks.Data.Bits.BitRead
  -- >>> import Data.Maybe
  --
  -- >>> closeAt (fromJust $ bitRead @Word8 "10101010") 1
  -- False
  --
  -- >>> closeAt (fromJust $ bitRead @Word8 "10101010") 2
  -- True
  --
  -- If the parenthesis at the given position does not exist in the input, it is considered to be a close.
  --
  -- >>> closeAt (fromJust $ bitRead @Word8 "10101010") 9
  -- True
  closeAt :: v -> Count -> Bool

instance (BitLength a, TestBit a) => CloseAt (BitShown a) where
  closeAt = closeAt' . bitShown
  {-# INLINE closeAt #-}

instance CloseAt [Bool] where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt (DVS.Vector Word8) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt (DVS.Vector Word16) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt (DVS.Vector Word32) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt (DVS.Vector Word64) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt Word8 where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt Word16 where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt Word32 where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt Word64 where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance CloseAt (Naive Word64) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt (Broadword Word64) where
  closeAt = closeAt . broadword
  {-# INLINE closeAt #-}
