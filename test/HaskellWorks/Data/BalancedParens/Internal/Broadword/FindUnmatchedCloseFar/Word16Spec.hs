{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word16Spec where

import Data.Maybe
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Numeric
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.FindClose                                       as C
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word16 as BW16
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Word16      as SW16
import qualified Hedgehog.Gen                                                                     as G
import qualified Hedgehog.Range                                                                   as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word16Spec" $ do
  it "findUnmatchedCloseFar 0 [11111111 11110100]" $ requireTest $ do
    p <- forAll $ pure 0
    w <- forAll $ pure $ fromJust $ bitRead "11111111 11110100"
    annotateShow $ bitShow w
    annotateShow $ showHex w ""
    BW16.findUnmatchedCloseFar p w === SW16.findUnmatchedCloseFar p w

  it "findUnmatchedCloseFar" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 0 16)
    w <- forAll $ G.word16 R.constantBounded
    annotateShow $ bitShow w
    BW16.findUnmatchedCloseFar p w === SW16.findUnmatchedCloseFar p w
  it "findClose" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 1 128)
    w <- forAll $ G.word16 R.constantBounded
    annotateShow $ bitShow w
    BW16.findClose w p === C.findClose w p