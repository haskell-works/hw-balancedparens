{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word8Spec where

import Control.Monad
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.Broadword.Word8     as W8
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.Word8 as SW8

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word8Spec" $ do
  describe "findCloseFar" $ do
    forM_ [0 .. 8] $ \p0 -> do
      forM_ [0 .. 0xff] $ \w0 -> do
        it ("word " <> bitShow w0) $ requireTest $ do
          p <- forAll $ pure p0
          w <- forAll $ pure w0
          W8.findCloseFar 0 (w .>. fromIntegral p) + p === SW8.findCloseFar p w
