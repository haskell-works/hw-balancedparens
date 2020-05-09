{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word32Spec where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.FindClose                                       as C
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word32 as BW32
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Word32      as SW32
import qualified Hedgehog.Gen                                                                     as G
import qualified Hedgehog.Range                                                                   as R

{- HLINT ignore "Evaluate"            -}
{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word32Spec" $ do
  it "findUnmatchedCloseFar" $ requireTest $ do
    p <- forAll $ pure 0
    w <- forAll $ pure 0xe9f6e7ff
    annotateShow $ bitShow w
    BW32.findUnmatchedCloseFar 0 p w === SW32.findUnmatchedCloseFar 0 p w
  it "findUnmatchedCloseFar" $ require $ withTests 40000 $ property $ do
    c <- forAll $ G.word64 (R.linear 0 64)
    p <- forAll $ G.word64 (R.linear 0 32)
    w <- forAll $ G.word32 R.constantBounded
    annotateShow $ bitShow w
    BW32.findUnmatchedCloseFar c p w === SW32.findUnmatchedCloseFar c p w
  it "findClose" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 1 128)
    w <- forAll $ G.word32 R.constantBounded
    annotateShow $ bitShow w
    BW32.findClose w p === C.findClose w p
