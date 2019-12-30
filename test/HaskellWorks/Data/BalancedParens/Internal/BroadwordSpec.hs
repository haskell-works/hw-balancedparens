{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.Internal.BroadwordSpec where

import Data.Word
import GHC.Generics
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.Broadword
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen as G
import qualified Hedgehog.Gen                         as G
import qualified Hedgehog.Range                       as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow, Generic)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.BroadwordSpec" $ do
  describe "For (()(()())) 1101101000" $ do
    let bs = Broadword (91 :: Word64)
    it "Test 1a" $ findClose bs  1 `shouldBe` Just 10
    it "Test 1b" $ findClose bs  2 `shouldBe` Just  3
    it "Test 1b" $ findClose bs  3 `shouldBe` Just  3
    it "Test 1b" $ findClose bs  4 `shouldBe` Just  9
    it "Test 1b" $ findClose bs  5 `shouldBe` Just  6
    it "Test 1b" $ findClose bs  6 `shouldBe` Just  6
    it "Test 1b" $ findClose bs  7 `shouldBe` Just  8
    it "Test 1b" $ findClose bs  8 `shouldBe` Just  8
    it "Test 1b" $ findClose bs  9 `shouldBe` Just  9
    it "Test 1b" $ findClose bs 10 `shouldBe` Just 10
    -- it "Test 2a" $ findOpen  bs 10 `shouldBe` Just  1
    -- it "Test 2b" $ findOpen  bs  3 `shouldBe` Just  2
    -- it "Test 3a" $ enclose   bs  2 `shouldBe` Just  1
    -- it "Test 3b" $ enclose   bs  7 `shouldBe` Just  4
  -- describe "For (()(()())) 1101101000" $ do
  --   let bs = Broadword (fromJust (bitRead "1101101000") :: [Bool])
  --   it "Test 1a" $ findClose bs  1 `shouldBe` Just 10
  --   it "Test 1b" $ findClose bs  2 `shouldBe` Just  3
  --   it "Test 1b" $ findClose bs  3 `shouldBe` Just  3
  --   it "Test 1b" $ findClose bs  4 `shouldBe` Just  9
  --   it "Test 1b" $ findClose bs  5 `shouldBe` Just  6
  --   it "Test 1b" $ findClose bs  6 `shouldBe` Just  6
  --   it "Test 1b" $ findClose bs  7 `shouldBe` Just  8
  --   it "Test 1b" $ findClose bs  8 `shouldBe` Just  8
  --   it "Test 1b" $ findClose bs  9 `shouldBe` Just  9
  --   it "Test 1b" $ findClose bs 10 `shouldBe` Just 10
    -- it "Test 2a" $ findOpen  bs 10 `shouldBe` Just  1
    -- it "Test 2b" $ findOpen  bs  3 `shouldBe` Just  2
    -- it "Test 3a" $ enclose   bs  2 `shouldBe` Just  1
    -- it "Test 3b" $ enclose   bs  7 `shouldBe` Just  4
    -- it "firstChild 1"   $ firstChild  bs 1 `shouldBe` Just 2
    -- it "firstChild 4"   $ firstChild  bs 4 `shouldBe` Just 5
    -- it "nextSibling 2"  $ nextSibling bs 2 `shouldBe` Just 4
    -- it "nextSibling 5"  $ nextSibling bs 5 `shouldBe` Just 7
    -- it "parent 2" $ parent  bs  2 `shouldBe` Just 1
    -- it "parent 5" $ parent  bs  5 `shouldBe` Just 4
    -- it "depth  1" $ depth   bs  1 `shouldBe` Just 1
    -- it "depth  2" $ depth   bs  2 `shouldBe` Just 2
    -- it "depth  3" $ depth   bs  3 `shouldBe` Just 2
    -- it "depth  4" $ depth   bs  4 `shouldBe` Just 2
    -- it "depth  5" $ depth   bs  5 `shouldBe` Just 3
    -- it "depth  6" $ depth   bs  6 `shouldBe` Just 3
    -- it "depth  7" $ depth   bs  7 `shouldBe` Just 3
    -- it "depth  8" $ depth   bs  8 `shouldBe` Just 3
    -- it "depth  9" $ depth   bs  9 `shouldBe` Just 2
    -- it "depth 10" $ depth   bs 10 `shouldBe` Just 1
    -- it "subtreeSize  1" $ subtreeSize bs  1 `shouldBe` Just 5
    -- it "subtreeSize  2" $ subtreeSize bs  2 `shouldBe` Just 1
    -- it "subtreeSize  3" $ subtreeSize bs  3 `shouldBe` Just 0
    -- it "subtreeSize  4" $ subtreeSize bs  4 `shouldBe` Just 3
    -- it "subtreeSize  5" $ subtreeSize bs  5 `shouldBe` Just 1
    -- it "subtreeSize  6" $ subtreeSize bs  6 `shouldBe` Just 0
    -- it "subtreeSize  7" $ subtreeSize bs  7 `shouldBe` Just 1
    -- it "subtreeSize  8" $ subtreeSize bs  8 `shouldBe` Just 0
    -- it "subtreeSize  9" $ subtreeSize bs  9 `shouldBe` Just 0
    -- it "subtreeSize 10" $ subtreeSize bs 10 `shouldBe` Just 0
  describe "For (()(()())) 11011010 00000000 :: DVS.Vector Word8" $ do
    let bs = Broadword (DVS.head (fromBitTextByteString "11011010 00000000") :: Word64)
    it "Test 1a" $ findClose bs  1 `shouldBe` Just 10
    it "Test 1b" $ findClose bs  2 `shouldBe` Just  3
    it "Test 1b" $ findClose bs  3 `shouldBe` Just  3
    it "Test 1b" $ findClose bs  4 `shouldBe` Just  9
    it "Test 1b" $ findClose bs  5 `shouldBe` Just  6
    it "Test 1b" $ findClose bs  6 `shouldBe` Just  6
    it "Test 1b" $ findClose bs  7 `shouldBe` Just  8
    it "Test 1b" $ findClose bs  8 `shouldBe` Just  8
    it "Test 1b" $ findClose bs  9 `shouldBe` Just  9
    it "Test 1b" $ findClose bs 10 `shouldBe` Just 10
    -- it "Test 2a" $ findOpen  bs 10 `shouldBe` Just  1
    -- it "Test 2b" $ findOpen  bs  3 `shouldBe` Just  2
    -- it "Test 3a" $ enclose   bs  2 `shouldBe` Just  1
    -- it "Test 3b" $ enclose   bs  7 `shouldBe` Just  4
    -- it "firstChild 1"  $ firstChild  bs 1 `shouldBe` Just 2
    -- it "firstChild 4"  $ firstChild  bs 4 `shouldBe` Just 5
    -- it "nextSibling 2" $ nextSibling bs 2 `shouldBe` Just 4
    -- it "nextSibling 5" $ nextSibling bs 5 `shouldBe` Just 7
    -- it "parent 2" $ parent bs 2 `shouldBe` Just 1
    -- it "parent 5" $ parent bs 5 `shouldBe` Just 4
    -- it "depth  1" $ depth bs  1 `shouldBe` Just 1
    -- it "depth  2" $ depth bs  2 `shouldBe` Just 2
    -- it "depth  3" $ depth bs  3 `shouldBe` Just 2
    -- it "depth  4" $ depth bs  4 `shouldBe` Just 2
    -- it "depth  5" $ depth bs  5 `shouldBe` Just 3
    -- it "depth  6" $ depth bs  6 `shouldBe` Just 3
    -- it "depth  7" $ depth bs  7 `shouldBe` Just 3
    -- it "depth  8" $ depth bs  8 `shouldBe` Just 3
    -- it "depth  9" $ depth bs  9 `shouldBe` Just 2
    -- it "depth 10" $ depth bs 10 `shouldBe` Just 1
    -- it "subtreeSize  1" $ subtreeSize bs  1 `shouldBe` Just 5
    -- it "subtreeSize  2" $ subtreeSize bs  2 `shouldBe` Just 1
    -- it "subtreeSize  3" $ subtreeSize bs  3 `shouldBe` Just 0
    -- it "subtreeSize  4" $ subtreeSize bs  4 `shouldBe` Just 3
    -- it "subtreeSize  5" $ subtreeSize bs  5 `shouldBe` Just 1
    -- it "subtreeSize  6" $ subtreeSize bs  6 `shouldBe` Just 0
    -- it "subtreeSize  7" $ subtreeSize bs  7 `shouldBe` Just 1
    -- it "subtreeSize  8" $ subtreeSize bs  8 `shouldBe` Just 0
    -- it "subtreeSize  9" $ subtreeSize bs  9 `shouldBe` Just 0
    -- it "subtreeSize 10" $ subtreeSize bs 10 `shouldBe` Just 0
  -- describe "Does not suffer exceptions" $ do
  --   it "when calling nextSibling from valid locations" $ do
  --     forAll (vectorSizedBetween 1 64) $ \(ShowVector v) -> do
  --       [nextSibling v p | p <- [1..bitLength v]] `shouldBe` [nextSibling v p | p <- [1..bitLength v]]
  it "Broadword findClose should behave the same as Naive findClose" $ requireProperty $ do
    w <- forAll $ G.word64 R.constantBounded
    p <- forAll $ G.count (R.linear 1 64)
    findClose w p === findClose (Broadword w) p
