{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.BalancedParens.FindCloseN
  ( FindCloseN(..)
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable                                              as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindCloseN.Generic as G

class FindCloseN v where
  -- | Find the position of the corresponding close parenthesis carrying in a number of open parentheses starting from a given position.
  --
  -- All positions are one based.
  --
  -- See the reference implementation 'G.findCloseN' for details
  findCloseN :: v -> Count -> Count -> Maybe Count

instance (CloseAt a, TestBit a, BitLength a) => FindCloseN (BitShown a) where
  findCloseN = G.findCloseN . bitShown
  {-# INLINE findCloseN #-}

instance FindCloseN [Bool] where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN  #-}

instance FindCloseN (DVS.Vector Word8) where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN  #-}

instance FindCloseN (DVS.Vector Word16) where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN #-}

instance FindCloseN (DVS.Vector Word32) where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN  #-}

instance FindCloseN (DVS.Vector Word64) where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN #-}

instance FindCloseN Word8 where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN #-}

instance FindCloseN Word16 where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN #-}

instance FindCloseN Word32 where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN  #-}

instance FindCloseN Word64 where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN #-}

instance FindCloseN (Naive Word64) where
  findCloseN = G.findCloseN
  {-# INLINE findCloseN #-}
