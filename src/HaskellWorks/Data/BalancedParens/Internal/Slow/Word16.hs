{-# LANGUAGE LambdaCase #-}

module HaskellWorks.Data.BalancedParens.Internal.Slow.Word16
  ( findCloseFar
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

findCloseFar :: Word16 -> Word16 -> Word16
findCloseFar = go 0
  where go :: Word16 -> Word16 -> Word16 -> Word16
        go d 16 _ = 16 + d
        go d i w = case (w .>. fromIntegral i) .&. 1 of
          1 -> go (d + 1) (i + 1) w
          _ -> if d == 0
            then i
            else go (d - 1) (i + 1) w
