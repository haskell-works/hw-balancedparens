{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.RangeMinSpec where

import Data.Word
import GHC.Generics
import HaskellWorks.Data.BalancedParens
import HaskellWorks.Data.BalancedParens.RangeMin
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen as G
import qualified Hedgehog.Gen                         as G
import qualified Hedgehog.Range                       as R

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow, Generic)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

factor :: Int
factor = 16384
{-# INLINE factor #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.RangeMinSpec" $ do
  it "For a simple bit string can find close" $ requireTest $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rm = mkRangeMin v
    findClose rm 61 === findClose v 61
  it "findClose should return the same result" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 4) (G.word64 R.constantBounded)
    let !rm = mkRangeMin v
    let len = bitLength v
    [findClose rm i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result over all counts" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkRangeMin v
    findClose rm p === findClose v p
  it "nextSibling should return the same result" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    let !rm = mkRangeMin v
    nextSibling rm 0 === nextSibling v 0
  it "nextSibling should return the same result over all counts" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkRangeMin v
    nextSibling rm p === nextSibling v p
