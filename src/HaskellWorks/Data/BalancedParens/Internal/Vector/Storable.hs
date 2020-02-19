module HaskellWorks.Data.BalancedParens.Internal.Vector.Storable
  ( lastOrZero
  , reword
  , dropTake
  , dropTakeFill
  , pageFill
  ) where

import qualified Data.Vector.Storable as DVS

lastOrZero :: (DVS.Storable a, Integral a) => DVS.Vector a -> a
lastOrZero v = if not (DVS.null v) then DVS.last v else 0
{-# INLINE lastOrZero #-}

reword :: (DVS.Storable a, Integral a, DVS.Storable b, Num b) => DVS.Vector a -> DVS.Vector b
reword v = DVS.generate (DVS.length v) (\i -> fromIntegral (v DVS.! i))
{-# INLINE reword #-}

dropTake :: DVS.Storable a => Int -> Int -> DVS.Vector a -> DVS.Vector a
dropTake n o = DVS.take o . DVS.drop n
{-# INLINE dropTake #-}

dropTakeFill :: DVS.Storable a => Int -> Int -> a -> DVS.Vector a -> DVS.Vector a
dropTakeFill n o a v =  let r = DVS.take o (DVS.drop n v) in
                        let len = DVS.length r in
                        if len == o then r else DVS.concat [r, DVS.fromList (replicate (o - len) a)]
{-# INLINE dropTakeFill #-}

-- | Return the n-th page of size s from the input vector.  In the case where there isn't
-- sufficient data to fill the page from the input vector, then the remainder of the page
-- is filled with a.
pageFill :: DVS.Storable a
  => Int          -- ^ The n-th page to retrieve
  -> Int          -- ^ The page size
  -> a            -- ^ The element value to fill the page when input vector has insufficient values
  -> DVS.Vector a -- ^ The input vector
  -> DVS.Vector a -- ^ The page
pageFill n s = dropTakeFill (n * s) s
{-# INLINE pageFill #-}
