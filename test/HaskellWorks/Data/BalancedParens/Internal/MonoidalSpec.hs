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
  it "drop2 should drop2 the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let rmm = RMM.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (RMM.size rmm))

    RMM.size (RMM.drop2 n rmm) === RMM.size rmm - n
  it "firstChild should choose the first child" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let rmm = RMM.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (RMM.size rmm))

    RMM.size (RMM.drop n rmm) === RMM.size rmm - n
  it "rose tree should be generatable" $ requireProperty $ do
    bs <- forAll $ G.balancedParens (R.linear 1 1000)

    RMM.toBools (RMM.fromBools bs) === bs
  it "firstChild should select first child" $ requireProperty $ do
    bs        <- forAll $ G.balancedParens (R.linear 1 1000)
    _         <- forAll $ pure (G.showBps bs)
    nodeCount <- forAll $ pure (fromIntegral (length bs `div` 2))
    ranked    <- forAll $ G.count (R.linear 1 nodeCount)
    pos       <- forAll $ pure $ select1 bs ranked
    rmm       <- forAll $ pure $ RMM.fromBools bs

    RMM.firstChild rmm pos === BP.firstChild bs pos
  it "firstChild should select first child" $ requireTest $ do
    let bps = "((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"
    let bs  = fmap (\c -> if c == '(' then True else False) bps
    let rmm = RMM.fromBools bs

    RMM.firstChild rmm 64 === Just 65
