{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Word
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Type
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Ops

import qualified Data.Vector.Storable                                                             as DVS
import qualified HaskellWorks.Data.BalancedParens.Gen                                             as G
import qualified HaskellWorks.Data.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word64 as BW64
import qualified HaskellWorks.Data.BalancedParens.Internal.Slow.Word64                            as SW64
import qualified HaskellWorks.Data.BalancedParens.ParensSeq                                       as PS
import qualified HaskellWorks.Data.BalancedParens.RangeMin                                        as RM
import qualified HaskellWorks.Data.BalancedParens.RangeMin2                                       as RM2
import qualified Hedgehog.Gen                                                                     as G
import qualified Hedgehog.Range                                                                   as R

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvRmVector :: Int -> IO (RM.RangeMin (DVS.Vector Word64))
setupEnvRmVector n = return $ RM.mkRangeMin $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvRm2Vector :: Int -> IO (RM2.RangeMin2 (DVS.Vector Word64))
setupEnvRm2Vector n = return $ RM2.mkRangeMin2 $ DVS.fromList (take n (cycle [maxBound, 0]))

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

benchWord64 :: [Benchmark]
benchWord64 = foldMap mkBenchWord64Group [0 .. 64]
  where mkBenchWord64Group :: Word64 -> [Benchmark]
        mkBenchWord64Group p = let q = (1 .<. p) - 1 in
          [ bgroup "Word64"
            [ bench ("Broadword find close " <> bitShow q) (whnf (BW64.findUnmatchedCloseFar 0) q)
            , bench ("Naive     find close " <> bitShow q) (whnf (SW64.findUnmatchedCloseFar 0) q)
            ]
          ]

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

benchRm :: [Benchmark]
benchRm =
  [ bgroup "Rm"
    [ env (G.sample (G.storableVector (R.singleton 1000) (G.word64 R.constantBounded))) $ \v -> bgroup "Vector64"
      [ bench "mkRangeMin"        (nf   RM.mkRangeMin v)
      ]
    , env (setupEnvRmVector 1000000) $ \bv -> bgroup "RangeMin"
      [ bench "findClose"         (nf   (map (findClose bv)) [0, 1000..10000000])
      ]
    ]
  ]

benchRm2 :: [Benchmark]
benchRm2 =
  [ bgroup "Rm2"
    [ env (G.sample (G.storableVector (R.singleton 1000) (G.word64 R.constantBounded))) $ \v -> bgroup "Vector64"
      [ bench "mkRangeMin2"       (nf   RM2.mkRangeMin2 v)
      ]
    , env (setupEnvRm2Vector 1000000) $ \bv -> bgroup "RangeMin2"
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
  <> benchWord64
  <> benchVector
  <> benchRm
  <> benchRm2
  <> benchParensSeq
