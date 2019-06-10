{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.Internal.ParensSeqSpec where

import Data.Semigroup                                      ((<>))
import HaskellWorks.Data.BalancedParens.Internal.ParensSeq ((<|), (><), (|>))
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.BalancedParens     as BP
import qualified HaskellWorks.Data.BalancedParens.Gen                as G
import qualified HaskellWorks.Data.BalancedParens.Internal.List      as L
import qualified HaskellWorks.Data.BalancedParens.Internal.ParensSeq as PS
import qualified Hedgehog.Gen                                        as G
import qualified Hedgehog.Range                                      as R

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.ParensSeqSpec" $ do
  it "fromWord64s should produce Rmm of the right size" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    PS.size (PS.fromWord64s ws) === fromIntegral (length ws * 64)
  it "fromWord64s should produce Rmm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    PS.size (PS.fromPartialWord64s wns) === sum (snd <$> wns)
  it "fromWord64s should produce Rmm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    PS.toPartialWord64s (PS.fromWord64s ws) === zip ws (repeat 64)
  it "fromPartialWord64s should produce Rmm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    PS.toPartialWord64s (PS.fromPartialWord64s wns) === wns
  it "fromBools should produce Rmm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    PS.toPartialWord64s (PS.fromBools (L.toBools ws)) === zip ws (repeat 64)
  it "drop should drop the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let rmm = PS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (PS.size rmm))

    PS.size (PS.drop n rmm) === PS.size rmm - n
  it "firstChild should choose the first child" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let rmm = PS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (PS.size rmm))

    PS.size (PS.drop n rmm) === PS.size rmm - n
  it "rose tree should be generatable" $ requireProperty $ do
    bs <- forAll $ G.bpBools (R.linear 1 1000)

    PS.toBools (PS.fromBools bs) === bs
  it "firstChild should select first child" $ requireProperty $ do
    bs        <- forAll $ G.bpBools (R.linear 1 1000)
    _         <- forAll $ pure (G.showBps bs)
    nodeCount <- forAll $ pure (fromIntegral (length bs `div` 2))
    ranked    <- forAll $ G.count (R.linear 1 nodeCount)
    pos       <- forAll $ pure $ select1 bs ranked
    rmm       <- forAll $ pure $ PS.fromBools bs

    PS.firstChild rmm pos === BP.firstChild bs pos
  it "firstChild should select first child" $ requireTest $ do
    let bps = "((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"
    let bs  = fmap (\c -> if c == '(' then True else False) bps
    let rmm = PS.fromBools bs

    PS.firstChild rmm 64 === Just 65
  it "nextSibling should select next sibling" $ requireTest $ do
    bs        <- forAll $ G.bpBools (R.linear 1 1000)
    nodeCount <- forAll $ pure (fromIntegral (length bs `div` 2))
    ranked    <- forAll $ G.count (R.linear 1 nodeCount)
    pos       <- forAll $ pure $ select1 bs ranked
    rmm       <- forAll $ pure $ PS.fromBools bs

    PS.nextSibling rmm pos === BP.nextSibling bs pos
  it "nextSibling on ()()" $ requireTest $ do
    bs        <- forAll $ pure [True , False , True , False]
    pos       <- forAll $ pure 1
    rmm       <- forAll $ pure $ PS.fromBools bs

    PS.nextSibling rmm pos === BP.nextSibling bs pos
  it "(><) should append" $ requireTest $ do
    bs1       <- forAll $ G.bpBools (R.linear 1 1000)
    bs2       <- forAll $ G.bpBools (R.linear 1 1000)
    ps1       <- forAll $ pure $ PS.fromBools bs1
    ps2       <- forAll $ pure $ PS.fromBools bs2

    PS.toBools (ps1 >< ps2) === PS.toBools ps1 >< PS.toBools ps2
  it "(<|) should cons" $ requireTest $ do
    b         <- forAll $ G.bool
    bs        <- forAll $ G.bpBools (R.linear 1 1000)
    ps        <- forAll $ pure $ PS.fromBools bs

    PS.toBools (b <| ps) === b:PS.toBools ps
  it "(|>) should snoc" $ requireTest $ do
    b         <- forAll $ G.bool
    bs        <- forAll $ G.bpBools (R.linear 1 1000)
    ps        <- forAll $ pure $ PS.fromBools bs

    PS.toBools (ps |> b) === PS.toBools ps <> [b]
