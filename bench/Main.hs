{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Semigroup                               ((<>))
import Data.Word
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.Bits.Broadword
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Ops

import qualified Data.Vector.Storable                                as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen                as G
import qualified HaskellWorks.Data.BalancedParens.Internal.ParensSeq as PS
import qualified HaskellWorks.Data.BalancedParens.RangeMinMax        as RMM
import qualified HaskellWorks.Data.BalancedParens.RangeMinMax2       as RMM2
import qualified Hedgehog.Gen                                        as G
import qualified Hedgehog.Range                                      as R

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvRmmVector :: Int -> IO (RMM.RangeMinMax (DVS.Vector Word64))
setupEnvRmmVector n = return $ RMM.mkRangeMinMax $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvRmm2Vector :: Int -> IO (RMM2.RangeMinMax2 (DVS.Vector Word64))
setupEnvRmm2Vector n = return $ RMM2.mkRangeMinMax2 $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvBP2 :: IO Word64
setupEnvBP2 = return $ DVS.head (fromBitTextByteString "10")

setupEnvBP4 :: IO Word64
setupEnvBP4 = return $ DVS.head (fromBitTextByteString "1100")

setupEnvBP8 :: IO Word64
setupEnvBP8 = return $ DVS.head (fromBitTextByteString "11101000")

setupEnvBP16 :: IO Word64
setupEnvBP16 = return $ DVS.head (fromBitTextByteString "11111000 11100000")

setupEnvBP32 :: IO Word64
setupEnvBP32 = return $ DVS.head (fromBitTextByteString "11111000 11101000 11101000 11100000")

setupEnvBP64 :: IO Word64
setupEnvBP64 = return $ DVS.head (fromBitTextByteString "11111000 11101000 11101000 11101000 11101000 11101000 11101000 11100000")

benchVector :: [Benchmark]
benchVector =
  [ bgroup "Vector"
    [ env setupEnvBP2 $ \w -> bgroup "FindClose 2-bit"
      [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
      , bench "Naive"         (whnf (findClose (Naive     w)) 1)
      ]
    , env setupEnvBP4 $ \w -> bgroup "FindClose 4-bit"
      [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
      , bench "Naive"         (whnf (findClose (Naive     w)) 1)
      ]
    , env setupEnvBP8 $ \w -> bgroup "FindClose 8-bit"
      [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
      , bench "Naive"         (whnf (findClose (Naive     w)) 1)
      ]
    , env setupEnvBP16 $ \w -> bgroup "FindClose 16-bit"
      [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
      , bench "Naive"         (whnf (findClose (Naive     w)) 1)
      ]
    , env setupEnvBP32 $ \w -> bgroup "FindClose 32-bit"
      [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
      , bench "Naive"         (whnf (findClose (Naive     w)) 1)
      ]
    , env setupEnvBP64 $ \w -> bgroup "FindClose 64-bit"
      [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
      , bench "Naive"         (whnf (findClose (Naive     w)) 1)
      ]
    , env (setupEnvVector 1000000) $ \bv -> bgroup "Vanilla"
      [ bench "findClose"   (nf   (map (findClose bv)) [0, 1000..10000000])
      ]
    ]
  ]

benchRmm :: [Benchmark]
benchRmm =
  [ bgroup "Rmm"
    [ env (G.sample (G.storableVector (R.singleton 1000) (G.word64 R.constantBounded))) $ \v -> bgroup "Vector64"
      [ bench "mkRangeMinMax"     (nf   RMM.mkRangeMinMax v)
      ]
    , env (setupEnvRmmVector 1000000) $ \bv -> bgroup "RangeMinMax"
      [ bench "findClose"         (nf   (map (findClose bv)) [0, 1000..10000000])
      ]
    ]
  ]

benchRmm2 :: [Benchmark]
benchRmm2 =
  [ bgroup "Rmm2"
    [ env (G.sample (G.storableVector (R.singleton 1000) (G.word64 R.constantBounded))) $ \v -> bgroup "Vector64"
      [ bench "mkRangeMinMax2"    (nf   RMM2.mkRangeMinMax2 v)
      ]
    , env (setupEnvRmm2Vector 1000000) $ \bv -> bgroup "RangeMinMax2"
      [ bench "findClose"         (nf   (map (findClose bv)) [0, 1000..10000000])
      ]
    ]
  ]

benchParensSeq :: [Benchmark]
benchParensSeq =
  [ bgroup "ParensSeq"
    [ env (G.sample (G.bpParensSeq (R.singleton 100000))) $ \ps -> bgroup "ParensSeq"
      [ bench "firstChild"    (nf (map (PS.firstChild  ps)) [1,101..100000])
      , bench "nextSibling"   (nf (map (PS.nextSibling ps)) [1,101..100000])
      , bench "(<|)"          (nf (<| ps) True)
      , bench "(|>)"          (nf (ps |>) True)
      , bench "drop"          (nf (fmap (flip PS.drop  ps)) [1,101..100000])
      , bench "drop2"         (nf (fmap (flip PS.drop2 ps)) [1,101..100000])
      ]
    , env (G.sample (G.vec2 (G.bpParensSeq (R.singleton 100000)))) $ \ ~(ps1, ps2) -> bgroup "ParensSeq"
      [ bench "(<>)"          (nf (ps1 <>) ps2)
      ]
    , env (G.sample (G.list (R.singleton 100) (G.word64 (R.constantBounded)))) $ \ws -> bgroup "ParensSeq"
      [ bench "fromWord64s"   (nf PS.fromWord64s ws)
      ]
    ]
  ]

main :: IO ()
main = defaultMain $ mempty
  <> benchVector
  <> benchRmm
  <> benchRmm2
  <> benchParensSeq
