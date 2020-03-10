{-# LANGUAGE LambdaCase #-}

module HaskellWorks.Data.BalancedParens.Internal.Slow.Word8
  ( findCloseFar
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

findCloseFar :: Word8 -> Word8 -> Word8
findCloseFar = go 0
  where go :: Word8 -> Word8 -> Word8 -> Word8
        go d 8 _ = 8 + d
        go d i w = case (w .>. fromIntegral i) .&. 1 of
          1 -> go (d + 1) (i + 1) w
          _ -> if d == 0
            then i
            else go (d - 1) (i + 1) w
