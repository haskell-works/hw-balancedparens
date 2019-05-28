module HaskellWorks.Data.BalancedParens.Internal.Word
  ( toBoolsDiff
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

toBoolsDiff :: Word64 -> [Bool] -> [Bool]
toBoolsDiff w =
  (go 0x00) . (go 0x01) . (go 0x02) . (go 0x03) . (go 0x04) . (go 0x05) . (go 0x06) . (go 0x07) .
  (go 0x08) . (go 0x09) . (go 0x0a) . (go 0x0b) . (go 0x0c) . (go 0x0d) . (go 0x0e) . (go 0x0f) .
  (go 0x10) . (go 0x11) . (go 0x12) . (go 0x13) . (go 0x14) . (go 0x15) . (go 0x16) . (go 0x17) .
  (go 0x18) . (go 0x19) . (go 0x1a) . (go 0x1b) . (go 0x1c) . (go 0x1d) . (go 0x1e) . (go 0x1f) .
  (go 0x20) . (go 0x21) . (go 0x22) . (go 0x23) . (go 0x24) . (go 0x25) . (go 0x26) . (go 0x27) .
  (go 0x28) . (go 0x29) . (go 0x2a) . (go 0x2b) . (go 0x2c) . (go 0x2d) . (go 0x2e) . (go 0x2f) .
  (go 0x30) . (go 0x31) . (go 0x32) . (go 0x33) . (go 0x34) . (go 0x35) . (go 0x36) . (go 0x37) .
  (go 0x38) . (go 0x39) . (go 0x3a) . (go 0x3b) . (go 0x3c) . (go 0x3d) . (go 0x3e) . (go 0x3f)
  where go :: Position -> [Bool] -> [Bool]
        go p = ((w .?. p):)
