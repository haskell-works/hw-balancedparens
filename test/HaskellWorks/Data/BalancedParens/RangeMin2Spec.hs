{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.RangeMin2Spec where

import Data.Word
import HaskellWorks.Data.BalancedParens
import HaskellWorks.Data.BalancedParens.RangeMin2
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen as G
import qualified Hedgehog.Range                       as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

factor :: Int
factor = 16384
{-# INLINE factor #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.RangeMinSpec2" $ do
  it "For a simple bit string can find close" $ requireTest $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rm = mkRangeMin2 v
    findClose rm 61 === findClose v 61
  it "findClose should return the same result" $ requireProperty $ do
    v <- forAll $ G.storableVectorWord64 (R.linear 1 4)
    let !rm = mkRangeMin2 v
    let len = bitLength v
    [findClose rm i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result over all counts" $ requireProperty $ do
    v <- forAll $ G.storableVectorWord64 (R.linear 1 factor)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkRangeMin2 v
    findClose rm p === findClose v p
  it "nextSibling should return the same result" $ requireProperty $ do
    v <- forAll $ G.storableVectorWord64 (R.linear 1 factor)
    let !rm = mkRangeMin2 v
    nextSibling rm 0 === nextSibling v 0
  it "nextSibling should return the same result over all counts" $ requireProperty $ do
    v <- forAll $ G.storableVectorWord64 (R.linear 1 factor)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkRangeMin2 v
    nextSibling rm p === nextSibling v p
