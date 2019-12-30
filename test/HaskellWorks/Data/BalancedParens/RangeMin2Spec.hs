{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.RangeMin2Spec where

import Data.Word
import GHC.Generics
import HaskellWorks.Data.BalancedParens
import HaskellWorks.Data.BalancedParens.RangeMin
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.FromBitTextByteString
import Test.Hspec

import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow, Generic)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

maxVectorSize :: Int
maxVectorSize = 16384
{-# INLINE maxVectorSize #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.RangeMinSpec2" $ do
  -- let maxSuccessDefault = 5
  it "For a simple bit string can find close" $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rm = mkRangeMin v
    findClose rm 61 `shouldBe` findClose v 61
  -- it "findClose should return the same result" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 4) $ \(ShowVector v) -> do
  --       let !rm = mkRangeMin v
  --       let len = bitLength v
  --       [findClose rm i | i <- [1..len]] `shouldBe `[findClose v i | i <- [1..len]]
  -- it "findClose should return the same result over all counts" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       forAll (choose (1, bitLength v)) $ \p -> do
  --         let !rm = mkRangeMin v
  --         findClose rm p `shouldBe` findClose v p
  -- it "nextSibling should return the same result" $ do
  --   forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --     let !rm = mkRangeMin v
  --     nextSibling rm 0 `shouldBe` nextSibling v 0
  -- it "nextSibling should return the same result over all counts" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       forAll (choose (1, bitLength v)) $ \p -> do
  --         let !rm = mkRangeMin v
  --         nextSibling rm p `shouldBe` nextSibling v p
  -- it "rangeMinBP should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2BP rm2 `shouldBe` rangeMinBP rm1
  -- it "rangeMinL0Excess should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L0Excess rm2 `shouldBe` rangeMinL0Excess rm1
  -- it "rangeMinL0Min should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L0Min rm2 `shouldBe` rangeMinL0Min rm1
  -- it "rangeMinL0Max should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L0Max rm2 `shouldBe` rangeMinL0Max rm1
  -- it "rangeMinL1Min should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L1Min rm2 `shouldBe` rangeMinL1Min rm1
  -- it "rangeMinL1Max should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L1Max rm2 `shouldBe` rangeMinL1Max rm1
  -- it "rangeMinL1Excess should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L1Excess rm2 `shouldBe` rangeMinL1Excess rm1
  -- it "rangeMinL2Min should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L2Min rm2 `shouldBe` rangeMinL2Min rm1
  -- it "rangeMinL2Max should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L2Max rm2 `shouldBe` rangeMinL2Max rm1
  -- it "rangeMinL2Excess should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rm1 = mkRangeMin   v
  --       let !rm2 = mkRangeMin2  v
  --       rangeMin2L2Excess rm2 `shouldBe` rangeMinL2Excess rm1
  -- describe "For example long bit string" $ do
  --   let v = fromBitTextByteString " \
  --     \ 01101101 01111100 10011111 01100101 11111100 01101111 00000000 00000000 10001010 11000000 01000010 01010010 01001101 01000101 00000000 00000000 \
  --     \ " :: DVS.Vector Word64
  --   let !rm1 = mkRangeMin   v
  --   let !rm2 = mkRangeMin2  v
  --   it "l0 max matches" $ do
  --     rangeMin2L0Max rm2 `shouldBe` rangeMinL0Max rm1
  --   it "l1 max matches" $ do
  --     rangeMin2L1Max rm2 `shouldBe` rangeMinL1Max rm1
  --   -- it "l2 max matches" $ do
  --   --   rangeMin2L2Max rm2 `shouldBe` rangeMinL2Max rm1
  --   it "l0 min matches" $ do
  --     rangeMin2L0Min rm2 `shouldBe` rangeMinL0Min rm1
  --   it "l1 min matches" $ do
  --     rangeMin2L1Min rm2 `shouldBe` rangeMinL1Min rm1
  --   it "l2 min matches" $ do
  --     rangeMin2L2Min rm2 `shouldBe` rangeMinL2Min rm1
