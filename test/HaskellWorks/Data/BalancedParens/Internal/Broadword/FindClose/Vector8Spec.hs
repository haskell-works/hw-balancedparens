{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector8Spec
  ( spec
  ) where

import Control.Monad
import Data.Word
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Int.Widen
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List                                                             as L
import qualified Data.Vector.Storable                                                  as DVS
import qualified HaskellWorks.Data.BalancedParens.FindClose                            as CLS
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector8 as V8
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindCloseN.Generic     as G
import qualified HaskellWorks.Data.BalancedParens.RangeMin2                            as RM2
import qualified HaskellWorks.Data.FromForeignRegion                                   as IO
import qualified Hedgehog.Gen                                                          as G
import qualified Hedgehog.Range                                                        as R
import qualified System.Directory                                                      as IO
import qualified System.IO.Unsafe                                                      as IO

{-# ANN module ("HLint: ignore Evaluate"            :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

testFiles :: [FilePath]
testFiles = IO.unsafePerformIO $ do
  files <- IO.listDirectory "data/test"
  return $ L.sort (("data/test/" ++) <$> (".ib.idx" `L.isSuffixOf`) `filter` files)
{-# NOINLINE testFiles #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector8Spec" $ do
  describe "findClose" $ do
    it "Two element vector zero as second word" $ require $ withTests 1000 $ property $ do
      w   <- forAll $ G.word8 R.constantBounded
      p   <- forAll $ G.word64 (R.linear 1 8)
      _   <- forAll $ pure $ bitShow w
      v   <- forAll $ pure $ DVS.fromList [w, 0]
      V8.findClose v p === G.findCloseN w 0 p
    it "Two element vector up to position 16" $ require $ withTests 1000 $ property $ do
      w0  <- forAll $ G.word8 R.constantBounded
      w1  <- forAll $ G.word8 R.constantBounded
      w   <- forAll $ pure $ id @Word16 $
                (widen w1 .<. (bitLength w0 * 1)) .|.
                (widen w0 .<. (bitLength w0 * 0))
      p   <- forAll $ G.word64 (R.linear 1 (bitLength w))
      _   <- forAll $ pure $ bitShow w
      v   <- forAll $ pure $ DVS.fromList [w0, w1]
      V8.findClose v p === G.findCloseN w 0 p
    it "Four element vector up to position 32" $ require $ withTests 1000 $ property $ do
      w0  <- forAll $ G.word8 R.constantBounded
      w1  <- forAll $ G.word8 R.constantBounded
      w2  <- forAll $ G.word8 R.constantBounded
      w3  <- forAll $ G.word8 R.constantBounded
      w   <- forAll $ pure $ id @Word32 $
                (widen w3 .<. (bitLength w0 * 3)) .|.
                (widen w2 .<. (bitLength w0 * 2)) .|.
                (widen w1 .<. (bitLength w0 * 1)) .|.
                (widen w0 .<. (bitLength w0 * 0))
      p   <- forAll $ G.word64 (R.linear 1 (bitLength w))
      _   <- forAll $ pure $ bitShow w
      v   <- forAll $ pure $ DVS.fromList [w0, w1, w2, w3]
      V8.findClose v p === G.findCloseN w 0 p
    it "Eight element vector up to position 64" $ require $ withTests 1000 $ property $ do
      w0  <- forAll $ G.word8 R.constantBounded
      w1  <- forAll $ G.word8 R.constantBounded
      w2  <- forAll $ G.word8 R.constantBounded
      w3  <- forAll $ G.word8 R.constantBounded
      w4  <- forAll $ G.word8 R.constantBounded
      w5  <- forAll $ G.word8 R.constantBounded
      w6  <- forAll $ G.word8 R.constantBounded
      w7  <- forAll $ G.word8 R.constantBounded
      w   <- forAll $ pure $ id @Word64 $
                (widen w7 .<. (bitLength w0 * 7)) .|.
                (widen w6 .<. (bitLength w0 * 6)) .|.
                (widen w5 .<. (bitLength w0 * 5)) .|.
                (widen w4 .<. (bitLength w0 * 4)) .|.
                (widen w3 .<. (bitLength w0 * 3)) .|.
                (widen w2 .<. (bitLength w0 * 2)) .|.
                (widen w1 .<. (bitLength w0 * 1)) .|.
                (widen w0 .<. (bitLength w0 * 0))
      p   <- forAll $ G.word64 (R.linear 1 (bitLength w))
      _   <- forAll $ pure $ bitShow w
      v   <- forAll $ pure $ DVS.fromList [w0, w1, w2, w3, w4, w5, w6, w7]
      V8.findClose v p === G.findCloseN w 0 p
    it "Two element vector" $ require $ withTests 1000 $ property $ do
      w0  <- forAll $ G.word8 R.constantBounded
      w1  <- forAll $ G.word8 R.constantBounded
      p   <- forAll $ G.word64 (R.linear 1 (bitLength w0 * 2))
      v   <- forAll $ pure $ DVS.fromList [w0, w1]
      _   <- forAll $ pure $ bitShow v
      V8.findClose v p === G.findCloseN v 0 p
    describe "Corpus tests" $ do
      forM_ testFiles $ \file -> do
        it ("File " <> file) $ do
          v     <- IO.mmapFromForeignRegion file
          rmm2  <- pure $ RM2.mkRangeMin2 (v :: DVS.Vector Word64)
          require $ withTests 100000 $ property $ do
            _   <- forAll $ pure file
            p   <- forAll $ G.word64 (R.linear 1 (bitLength v))
            _   <- forAll $ pure $ bitShow v
            CLS.findClose v p === CLS.findClose rmm2 p
