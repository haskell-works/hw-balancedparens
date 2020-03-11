{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word64Spec where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.Broadword.Word64     as W64
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.Word64 as SW64
import qualified Hedgehog.Gen                                          as G
import qualified Hedgehog.Range                                        as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word64Spec" $ do
  it "findCloseFar" $ require $ withTests 100000 $ property $ do
    p <- forAll $ G.word64 (R.linear 0 64)
    w <- forAll $ G.word64 R.constantBounded
    annotateShow $ bitShow w
    W64.findCloseFar 0 (w .>. fromIntegral p) + p === SW64.findCloseFar p w
