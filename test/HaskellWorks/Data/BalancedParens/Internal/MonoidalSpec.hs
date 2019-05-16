{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.Internal.MonoidalSpec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.Gen                           as G
import qualified HaskellWorks.Data.BalancedParens.Internal.List                 as L
import qualified HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal as RMM
import qualified Hedgehog.Gen                                                   as G
import qualified Hedgehog.Range                                                 as R

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.MonoidalSpec" $ do
  it "fromWord64s should produce Rmm of the right size" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    RMM.size (RMM.fromWord64s ws) === fromIntegral (length ws * 64)
  it "fromWord64s should produce Rmm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    RMM.size (RMM.fromPartialWord64s wns) === sum (snd <$> wns)
  it "fromWord64s should produce Rmm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    RMM.toPartialWord64s (RMM.fromWord64s ws) === zip ws (repeat 64)
  it "fromPartialWord64s should produce Rmm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    RMM.toPartialWord64s (RMM.fromPartialWord64s wns) === wns
  it "fromBools should produce Rmm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    RMM.toPartialWord64s (RMM.fromBools (L.toBools ws)) === zip ws (repeat 64)
