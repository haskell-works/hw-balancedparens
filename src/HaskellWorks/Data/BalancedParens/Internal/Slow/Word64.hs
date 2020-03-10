{-# LANGUAGE LambdaCase #-}

module HaskellWorks.Data.BalancedParens.Internal.Slow.Word64
  ( findCloseFar
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

findCloseFar :: Word64 -> Word64 -> Word64
findCloseFar = go 0
  where go :: Word64 -> Word64 -> Word64 -> Word64
        go d 64 _ = 64 + d
        go d i w = case (w .>. fromIntegral i) .&. 1 of
          1 -> go (d + 1) (i + 1) w
          _ -> if d == 0
            then i
            else go (d - 1) (i + 1) w
