module HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector64Spec
  ( spec
  ) where

import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                                                   as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector64 as V64
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.FindCloseN.Generic      as G
import qualified Hedgehog.Gen                                                           as G
import qualified Hedgehog.Range                                                         as R

{- HLINT ignore "Evaluate"            -}
{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.Internal.Broadword.FindClose.Vector64Spec" $ do
  describe "findClose" $ do
    it "Two element vector zero as second word" $ require $ withTests 1000 $ property $ do
      w   <- forAll $ G.word64 R.constantBounded
      p   <- forAll $ G.word64 (R.linear 1 (bitLength w))
      _   <- forAll $ pure $ bitShow w
      v   <- forAll $ pure $ DVS.fromList [w, 0]
      V64.findClose v p === G.findCloseN w 0 p
    it "Two element vector" $ require $ withTests 1000 $ property $ do
      w0  <- forAll $ G.word64 R.constantBounded
      w1  <- forAll $ G.word64 R.constantBounded
      p   <- forAll $ G.word64 (R.linear 1 (bitLength w0 * 2))
      v   <- forAll $ pure $ DVS.fromList [w0, w1]
      _   <- forAll $ pure $ bitShow v
      V64.findClose v p === G.findCloseN v 0 p
