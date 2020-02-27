{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.BalancedParens.VectorGenSpec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.BalancedParens.Gen as G
import qualified Hedgehog.Gen                         as G
import qualified Hedgehog.Range                       as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.VectorGenSpec" $ do
  fit "Generate a vector" $ requireProperty $ do
    !_ <- forAll $ G.storableVector (R.linear 1 16384) (G.word64 R.constantBounded)
    return ()
