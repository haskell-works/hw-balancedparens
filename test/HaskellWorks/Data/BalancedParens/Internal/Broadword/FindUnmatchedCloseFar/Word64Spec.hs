{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word64Spec where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Naive
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.FindClose                                       as C
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Word64             as BW64
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word64 as BW64
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Word64      as SW64
import qualified Hedgehog.Gen                                                                     as G
import qualified Hedgehog.Range                                                                   as R

{- HLINT ignore "Redundant do"      -}
{- HLINT ignore "Redundant return"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word64Spec" $ do
  it "findUnmatchedCloseFar" $ require $ withTests 100000 $ property $ do
    c <- forAll $ G.word64 (R.linear 0 128)
    p <- forAll $ G.word64 (R.linear 0 128)
    w <- forAll $ G.word64 R.constantBounded
    annotateShow $ bitShow w
    BW64.findUnmatchedCloseFar c p w === SW64.findUnmatchedCloseFar c p w
  it "findClose" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 1 128)
    w <- forAll $ G.word64 R.constantBounded
    annotateShow $ bitShow w
    BW64.findClose w p === C.findClose (Naive w) p
