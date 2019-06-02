{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.Internal.MonoidalSpec where

import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.BalancedParens                as BP
import qualified HaskellWorks.Data.BalancedParens.Gen                           as G
import qualified HaskellWorks.Data.BalancedParens.Internal.List                 as L
import qualified HaskellWorks.Data.BalancedParens.Internal.RangeMinMax.Monoidal as RMM
import qualified HaskellWorks.Data.BalancedParens.Internal.RoseTree             as RT
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
  it "drop should drop the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let rmm = RMM.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (RMM.size rmm))

    RMM.size (RMM.drop n rmm) === RMM.size rmm - n
  it "firstChild should choose the first child" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let rmm = RMM.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (RMM.size rmm))

    RMM.size (RMM.drop n rmm) === RMM.size rmm - n
  it "rose tree should be generatable" $ requireProperty $ do
    rt <- forAll $ G.roseTree (R.linear 1 100)
    bs <- forAll $ pure $ RT.toBools rt

    RMM.toBools (RMM.fromBools bs) === bs
  it "rose tree should be generatable" $ requireProperty $ do
    rt        <- forAll $ G.roseTree (R.linear 1 2000)
    bs        <- forAll $ pure $ RT.toBools rt
    nodeCount <- forAll $ pure (fromIntegral (length bs `div` 2))
    ranked    <- forAll $ G.count (R.linear 1 nodeCount)
    pos       <- forAll $ pure $ select1 bs ranked
    rmm       <- forAll $ pure $ RMM.fromBools bs

    BP.firstChild bs pos === RMM.firstChild rmm pos
