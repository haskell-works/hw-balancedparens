{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.Broadword.Word8Spec where

import Control.Monad                  (forM_)
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.Broadword.Word8     as BW8
import qualified HaskellWorks.Data.BalancedParens.FindClose           as C
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.Word8 as SW8
import qualified Hedgehog.Gen                                         as G
import qualified Hedgehog.Range                                       as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Broadword.Word8Spec" $ do
  describe "findUnmatchedCloseFar" $ do
    forM_ [0 .. 8] $ \p0 -> do
      forM_ [0 .. 0xff] $ \w0 -> do
        it ("word " <> bitShow w0) $ requireTest $ do
          p <- forAll $ pure p0
          w <- forAll $ pure w0
          BW8.findUnmatchedCloseFar p w === SW8.findUnmatchedCloseFar p w
  it "findClose" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 1 128)
    w <- forAll $ G.word8 R.constantBounded
    annotateShow $ bitShow w
    BW8.findClose w p === C.findClose w p
