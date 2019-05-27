{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.BalancedParens.RangeMinMax2Spec where

import Data.Word
import HaskellWorks.Data.BalancedParens
import HaskellWorks.Data.BalancedParens.RangeMinMax
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.FromBitTextByteString
import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

vectorSizedBetween :: Int -> Int -> Gen (ShowVector (DVS.Vector Word64))
vectorSizedBetween a b = do
  n   <- choose (a, b)
  xs  <- sequence [ arbitrary | _ <- [1 .. n] ]
  return $ ShowVector (DVS.fromList xs)

maxVectorSize :: Int
maxVectorSize = 16384
{-# INLINE maxVectorSize #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.RangeMinMaxSpec2" $ do
  -- let maxSuccessDefault = 5
  it "For a simple bit string can find close" $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rmm = mkRangeMinMax v
    findClose rmm 61 `shouldBe` findClose v 61
  -- it "findClose should return the same result" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 4) $ \(ShowVector v) -> do
  --       let !rmm = mkRangeMinMax v
  --       let len = bitLength v
  --       [findClose rmm i | i <- [1..len]] `shouldBe `[findClose v i | i <- [1..len]]
  -- it "findClose should return the same result over all counts" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       forAll (choose (1, bitLength v)) $ \p -> do
  --         let !rmm = mkRangeMinMax v
  --         findClose rmm p `shouldBe` findClose v p
  -- it "nextSibling should return the same result" $ do
  --   forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --     let !rmm = mkRangeMinMax v
  --     nextSibling rmm 0 `shouldBe` nextSibling v 0
  -- it "nextSibling should return the same result over all counts" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       forAll (choose (1, bitLength v)) $ \p -> do
  --         let !rmm = mkRangeMinMax v
  --         nextSibling rmm p `shouldBe` nextSibling v p
  -- it "rangeMinMaxBP should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2BP rmm2 `shouldBe` rangeMinMaxBP rmm1
  -- it "rangeMinMaxL0Excess should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L0Excess rmm2 `shouldBe` rangeMinMaxL0Excess rmm1
  -- it "rangeMinMaxL0Min should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L0Min rmm2 `shouldBe` rangeMinMaxL0Min rmm1
  -- it "rangeMinMaxL0Max should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L0Max rmm2 `shouldBe` rangeMinMaxL0Max rmm1
  -- it "rangeMinMaxL1Min should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L1Min rmm2 `shouldBe` rangeMinMaxL1Min rmm1
  -- it "rangeMinMaxL1Max should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L1Max rmm2 `shouldBe` rangeMinMaxL1Max rmm1
  -- it "rangeMinMaxL1Excess should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L1Excess rmm2 `shouldBe` rangeMinMaxL1Excess rmm1
  -- it "rangeMinMaxL2Min should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L2Min rmm2 `shouldBe` rangeMinMaxL2Min rmm1
  -- it "rangeMinMaxL2Max should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L2Max rmm2 `shouldBe` rangeMinMaxL2Max rmm1
  -- it "rangeMinMaxL2Excess should match" $ do
  --   quickCheckWith stdArgs { maxSuccess = maxSuccessDefault } $ do
  --     forAll (vectorSizedBetween 1 maxVectorSize) $ \(ShowVector v) -> do
  --       let !rmm1 = mkRangeMinMax   v
  --       let !rmm2 = mkRangeMinMax2  v
  --       rangeMinMax2L2Excess rmm2 `shouldBe` rangeMinMaxL2Excess rmm1
  -- describe "For example long bit string" $ do
  --   let v = fromBitTextByteString " \
  --     \ 01101101 01111100 10011111 01100101 11111100 01101111 00000000 00000000 10001010 11000000 01000010 01010010 01001101 01000101 00000000 00000000 \
  --     \ " :: DVS.Vector Word64
  --   let !rmm1 = mkRangeMinMax   v
  --   let !rmm2 = mkRangeMinMax2  v
  --   it "l0 max matches" $ do
  --     rangeMinMax2L0Max rmm2 `shouldBe` rangeMinMaxL0Max rmm1
  --   it "l1 max matches" $ do
  --     rangeMinMax2L1Max rmm2 `shouldBe` rangeMinMaxL1Max rmm1
  --   -- it "l2 max matches" $ do
  --   --   rangeMinMax2L2Max rmm2 `shouldBe` rangeMinMaxL2Max rmm1
  --   it "l0 min matches" $ do
  --     rangeMinMax2L0Min rmm2 `shouldBe` rangeMinMaxL0Min rmm1
  --   it "l1 min matches" $ do
  --     rangeMinMax2L1Min rmm2 `shouldBe` rangeMinMaxL1Min rmm1
  --   it "l2 min matches" $ do
  --     rangeMinMax2L2Min rmm2 `shouldBe` rangeMinMaxL2Min rmm1
