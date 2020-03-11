{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word32Spec where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.Broadword.Word32     as W32
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.Word32 as SW32
import qualified Hedgehog.Gen                                          as G
import qualified Hedgehog.Range                                        as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word32Spec" $ do
  it "findCloseFar" $ requireTest $ do
    p <- forAll $ pure 0
    w <- forAll $ pure 0xe9f6e7ff
    annotateShow $ bitShow w
    W32.findCloseFar 0 (w .>. fromIntegral p) + p === SW32.findCloseFar p w
  it "findCloseFar" $ require $ withTests 10000 $ property $ do
    p <- forAll $ pure 0
    w <- forAll $ G.word32 R.constantBounded
    annotateShow $ bitShow w
    W32.findCloseFar 0 (w .>. fromIntegral p) + p === SW32.findCloseFar p w
