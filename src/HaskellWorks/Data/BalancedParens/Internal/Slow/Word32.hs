{-# LANGUAGE LambdaCase #-}

module HaskellWorks.Data.BalancedParens.Internal.Slow.Word32
  ( findCloseFar
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

findCloseFar :: Word32 -> Word32 -> Word32
findCloseFar = go 0
  where go :: Word32 -> Word32 -> Word32 -> Word32
        go d 32 _ = 32 + d
        go d i w = case (w .>. fromIntegral i) .&. 1 of
          1 -> go (d + 1) (i + 1) w
          _ -> if d == 0
            then i
            else go (d - 1) (i + 1) w
