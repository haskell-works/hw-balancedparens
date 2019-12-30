{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.Internal.ParensSeqSpec where

import Data.Semigroup                             ((<>))
import HaskellWorks.Data.BalancedParens.ParensSeq ((<|), (><), (|>))
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.BalancedParens as BP
import qualified HaskellWorks.Data.BalancedParens.Gen            as G
import qualified HaskellWorks.Data.BalancedParens.Internal.List  as L
import qualified HaskellWorks.Data.BalancedParens.ParensSeq      as PS
import qualified Hedgehog.Gen                                    as G
import qualified Hedgehog.Range                                  as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.ParensSeqSpec" $ do
  it "fromWord64s should produce Rm of the right size" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    PS.size (PS.fromWord64s ws) === fromIntegral (length ws * 64)
  it "fromWord64s should produce Rm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    PS.size (PS.fromPartialWord64s wns) === sum (snd <$> wns)
  it "fromWord64s should produce Rm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    PS.toPartialWord64s (PS.fromWord64s ws) === zip ws (repeat 64)
  it "fromPartialWord64s should produce Rm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    PS.toPartialWord64s (PS.fromPartialWord64s wns) === wns
  it "fromBools should produce Rm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    PS.toPartialWord64s (PS.fromBools (L.toBools ws)) === zip ws (repeat 64)
  it "drop should drop the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = PS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (PS.size ps))

    PS.size (PS.drop n ps) === PS.size ps - n
  it "take should take the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = PS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (PS.size ps))

    PS.size (PS.take n ps) === n
  it "splitAt should split at the correct point" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = PS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (PS.size ps))

    let (lt, rt) = PS.splitAt n ps
    PS.size lt === n
    PS.size rt === PS.size ps - n
    PS.toBools lt >< PS.toBools rt === PS.toBools ps
  it "firstChild should choose the first child" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = PS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (PS.size ps))

    PS.size (PS.drop n ps) === PS.size ps - n
  it "rose tree should be generatable" $ requireProperty $ do
    bs <- forAll $ G.bpBools (R.linear 1 1000)

    PS.toBools (PS.fromBools bs) === bs
  it "firstChild should select first child" $ requireProperty $ do
    bs        <- forAll $ G.bpBools (R.linear 1 1000)
    _         <- forAll $ pure (G.showBps bs)
    nodeCount <- forAll $ pure (fromIntegral (length bs `div` 2))
    ranked    <- forAll $ G.count (R.linear 1 nodeCount)
    pos       <- forAll $ pure $ select1 bs ranked
    ps        <- forAll $ pure $ PS.fromBools bs

    PS.firstChild ps pos === BP.firstChild bs pos
  it "firstChild should select first child" $ requireTest $ do
    let bps = "((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"
    let bs  = fmap (\c -> if c == '(' then True else False) bps
    let ps = PS.fromBools bs

    PS.firstChild ps 64 === Just 65
  it "nextSibling should select next sibling" $ requireTest $ do
    bs        <- forAll $ G.bpBools (R.linear 1 1000)
    nodeCount <- forAll $ pure (fromIntegral (length bs `div` 2))
    ranked    <- forAll $ G.count (R.linear 1 nodeCount)
    pos       <- forAll $ pure $ select1 bs ranked
    rm       <- forAll $ pure $ PS.fromBools bs

    PS.nextSibling rm pos === BP.nextSibling bs pos
  it "nextSibling on ()()" $ requireTest $ do
    bs        <- forAll $ pure [True , False , True , False]
    pos       <- forAll $ pure 1
    rm       <- forAll $ pure $ PS.fromBools bs

    PS.nextSibling rm pos === BP.nextSibling bs pos
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
