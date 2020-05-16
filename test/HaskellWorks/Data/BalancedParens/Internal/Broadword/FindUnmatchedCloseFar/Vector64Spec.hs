{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector64Spec where

import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                                                               as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen                                               as G
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector64 as BWV64
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Vector64      as SV64
import qualified Hedgehog.Gen                                                                       as G
import qualified Hedgehog.Range                                                                     as R

{- HLINT ignore "Evaluate"            -}
{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Vector64Spec" $ do
  it "findUnmatchedCloseFar against slow" $ requireTest $ do
    v   <- forAll $ pure DVS.empty
    c   <- forAll $ pure 0
    p   <- forAll $ pure 0
    annotateShow $ bitShow v
    actual    <- forAll $ pure $ BWV64.findUnmatchedCloseFar c p v
    expected  <- forAll $ pure $ SV64.findUnmatchedCloseFar  c p v
    actual === expected
  it "findUnmatchedCloseFar against slow" $ requireTest $ do
    v   <- forAll $ pure $ DVS.fromList [0]
    c   <- forAll $ pure 1
    p   <- forAll $ pure 8
    annotateShow $ bitShow v
    actual    <- forAll $ pure $ BWV64.findUnmatchedCloseFar c p v
    expected  <- forAll $ pure $ SV64.findUnmatchedCloseFar  c p v
    actual === expected
  it "findUnmatchedCloseFar against slow" $ require $ withTests 10000 $ property $ do
    v   <- forAll $ G.storableVector (R.linear 0 4) (G.word64 R.constantBounded)
    c   <- forAll $ G.word64 (R.linear 0 (bitLength v))
    p   <- forAll $ G.word64 (R.linear 0 (bitLength v))
    annotateShow $ bitShow v
    actual    <- forAll $ pure $ BWV64.findUnmatchedCloseFar c p v
    expected  <- forAll $ pure $ SV64.findUnmatchedCloseFar  c p v
    actual === expected
