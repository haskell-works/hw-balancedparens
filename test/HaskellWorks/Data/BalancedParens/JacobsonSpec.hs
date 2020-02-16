{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module HaskellWorks.Data.BalancedParens.JacobsonSpec where

import Data.Int
import Data.Word
import GHC.Generics
import HaskellWorks.Control.Monad.Lazy              (forceM)
import HaskellWorks.Data.BalancedParens
import HaskellWorks.Data.BalancedParens.Jacobson
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                      hiding (length)
import Test.Hspec

import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen as G
import qualified Hedgehog.Gen                         as G
import qualified Hedgehog.Range                       as R

{-# ANN module ("ignore Redundant do"        :: String) #-}
{-# ANN module ("ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow, Generic)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

factor :: Int
factor = 16384
{-# INLINE factor #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BalancedParens.JacobsonSpec" $ do
  it "For a simple bit string can find close" $ requireTest $ evalM $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rm = mkJacobson v
    findClose rm 61 === findClose v 61
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64 [0]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList []
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64 [0xffffffffffffffff, 0]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList [1]
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64 [0x00000000ffffffff, 0x00000000ffffffff]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList []
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64 [0x000000ffffffffff, 0x0000000000ffffff]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList [1]
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64 [0x000000ffffffffff, 0x00000000ffffffff, 0x0000000000ffffff]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList [2]
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64 [0x0000ffffffffffff, 0x0000000000ffffff, 0x0000000000ffffff]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList [1, 2]
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64
      [ 0x0000ffffffffffff
      , 0x0000000000ffffff
      , 0x0000000000ffffff
      , 0x0000ffffffffffff
      , 0x0000000000ffffff
      , 0x0000000000ffffff
      ]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList [1, 2, 4, 5]
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
    jacobsonOpenings2 jbp === DVS.fromList [0xffffffffffffffe3]
  it "findClose should return the same result" $ requireTest $ do
    v     <- forAll $ pure $ DVS.fromList @Word64
      [ 0x0000ffffffffffff
      , 0x0000000000ffffff
      , 0x0000000000ffffff
      , 0x0000ffffffffffff
      , 0x0000000000ffffff
      , 0x0000000000ffffff
      , 0x0000ffffffffffff
      , 0x0000000000ffffff
      , 0x0000000000ffffff
      , 0x0000ffffffffffff
      , 0x0000000000ffffff
      , 0x0000000000ffffff
      ]
    jbp   <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    jacobsonClosings jbp === DVS.fromList [1, 2, 4, 5, 7, 8, 10, 11]
    [findClose jbp i | i <- [1..len]] === [findClose v i | i <- [1..len]]
    jacobsonOpenings2 jbp === DVS.fromList [0xffffffffffffe223]
  it "findClose should return the same result" $ requireProperty $ do
    v     <- forAll $ G.storableVector (R.linear 1 4) (G.word64 R.constantBounded)
    rm    <- evalM $ forceM $ forAll $ pure $ mkJacobson v
    let len = bitLength v
    [findClose rm i | i <- [1..len]] === [findClose v i | i <- [1..len]]
  it "findClose should return the same result over all counts" $ requireProperty $ evalM $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkJacobson v
    findClose rm p === findClose v p
  it "nextSibling should return the same result" $ requireProperty $ evalM $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    let !rm = mkJacobson v
    nextSibling rm 0 === nextSibling v 0
  it "nextSibling should return the same result over all counts" $ requireProperty $ evalM $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkJacobson v
    nextSibling rm p === nextSibling v p
  it "jacobson 1" $ requireTest $ evalM $ do
    let v = DVS.fromList [0xffffffffffffffff, 0x0000000000000000] :: DVS.Vector Word64
    let !rm = mkJacobson v
    annotateShow rm
    rm === Jacobson
       { jacobsonBP         = v
       , jacobsonL0Min      = DVS.fromList [0 , -64]
       , jacobsonL0Excess   = DVS.fromList [64, -64]
       , jacobsonOpenings   = DVS.fromList [0]
       , jacobsonOpenings2  = DVS.fromList [0xffffffffffffffff]
       , jacobsonClosings   = DVS.fromList [1]
       }
