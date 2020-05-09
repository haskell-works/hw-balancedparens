{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.FindCloseNSpec where

import HaskellWorks.Data.BalancedParens
import HaskellWorks.Data.Bits.Broadword.Type
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.FindCloseNSpec" $ do
  it "returns same result as broadword" $ requireProperty $ do
    w <- forAll $ G.word64 R.constantBounded
    p <- forAll $ G.word64 (R.linear 1 64)
    findClose w p === findClose (Broadword w) p
