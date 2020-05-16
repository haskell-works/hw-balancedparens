{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector8Spec where

import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Int.Widen
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                                                              as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen                                              as G
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector8 as BWV8
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word16  as BWW16
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Vector8      as SV8
import qualified Hedgehog.Gen                                                                      as G
import qualified Hedgehog.Range                                                                    as R

{- HLINT ignore "Evaluate"            -}
{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector8Spec" $ do
  it "findUnmatchedCloseFar against two words" $ require $ withTests 1000 $ property $ do
    c   <- forAll $ G.word64 (R.linear 0 64)
    p   <- forAll $ G.word64 (R.linear 0 8)
    w0  <- forAll $ G.word8 R.constantBounded
    w1  <- forAll $ G.word8 R.constantBounded
    w   <- forAll $ pure $ (widen w1 .<. bitLength w0) .|. widen w0
    v   <- forAll $ pure $ DVS.fromList [w0, w1]
    annotateShow $ bitShow w
    actual    <- forAll $ pure $ BWV8.findUnmatchedCloseFar  c p v
    expected  <- forAll $ pure $ BWW16.findUnmatchedCloseFar c p w
    actual === expected
  it "findUnmatchedCloseFar against slow" $ requireTest $ do
    v   <- forAll $ pure DVS.empty
    c   <- forAll $ pure 0
    p   <- forAll $ pure 0
    annotateShow $ bitShow v
    actual    <- forAll $ pure $ BWV8.findUnmatchedCloseFar c p v
    expected  <- forAll $ pure $ SV8.findUnmatchedCloseFar  c p v
    actual === expected
  it "findUnmatchedCloseFar against slow" $ requireTest $ do
    v   <- forAll $ pure $ DVS.fromList [0]
    c   <- forAll $ pure 1
    p   <- forAll $ pure 8
    annotateShow $ bitShow v
    actual    <- forAll $ pure $ BWV8.findUnmatchedCloseFar c p v
    expected  <- forAll $ pure $ SV8.findUnmatchedCloseFar  c p v
    actual === expected
  it "findUnmatchedCloseFar against slow" $ require $ withTests 10000 $ property $ do
    v   <- forAll $ G.storableVector (R.linear 0 4) (G.word8 R.constantBounded)
    c   <- forAll $ G.word64 (R.linear 0 (bitLength v))
    p   <- forAll $ G.word64 (R.linear 0 (bitLength v))
    annotateShow $ bitShow v
    actual    <- forAll $ pure $ BWV8.findUnmatchedCloseFar c p v
    expected  <- forAll $ pure $ SV8.findUnmatchedCloseFar  c p v
    actual === expected
