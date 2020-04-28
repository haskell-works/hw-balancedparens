{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word64Spec where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Naive
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.FindClose                                       as C
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word64 as BW64
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Word64      as SW64
import qualified Hedgehog.Gen                                                                     as G
import qualified Hedgehog.Range                                                                   as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word64Spec" $ do
  it "findUnmatchedCloseFar" $ require $ withTests 100000 $ property $ do
    p <- forAll $ G.word64 (R.linear 0 64)
    w <- forAll $ G.word64 R.constantBounded
    annotateShow $ bitShow w
    BW64.findUnmatchedCloseFar p w === SW64.findUnmatchedCloseFar p w
  it "findClose" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 1 128)
    w <- forAll $ G.word64 R.constantBounded
    annotateShow $ bitShow w
    BW64.findClose w p === C.findClose (Naive w) p