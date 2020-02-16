{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.BalancedParens.Jacobson
  ( Jacobson(..)
  , mkJacobson
  , next
  , push
  , pop
  , pageSizeL0
  ) where

import Control.DeepSeq
import Control.Monad.ST                                (ST)
import Data.Int
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.BalancedParens.Enclose
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.BalancedParens.FindCloseN
import HaskellWorks.Data.BalancedParens.FindOpen
import HaskellWorks.Data.BalancedParens.FindOpenN
import HaskellWorks.Data.BalancedParens.NewCloseAt
import HaskellWorks.Data.BalancedParens.OpenAt
import HaskellWorks.Data.Bits.AllExcess.AllExcess1
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.MinExcess
import HaskellWorks.Data.Excess.MinExcess1
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                         hiding (length)

import qualified Data.Vector.Storable                                      as DVS
import qualified Data.Vector.Storable.Mutable                              as DVSM
import qualified HaskellWorks.Data.BalancedParens.Internal.Vector.Storable as DVS
import qualified HaskellWorks.Data.Vector.Storable                         as DVS

data Jacobson = Jacobson
  { jacobsonBP        :: !(DVS.Vector Word64)
  , jacobsonL0Min     :: !(DVS.Vector Int8)
  , jacobsonL0Excess  :: !(DVS.Vector Int8)
  , jacobsonOpenings  :: !(DVS.Vector Int)
  , jacobsonOpenings2 :: !(DVS.Vector Word64)
  , jacobsonClosings  :: !(DVS.Vector Int)
  } deriving (Eq, Show, NFData, Generic)

factorL0 :: Integral a => a
factorL0 = 1
{-# INLINE factorL0 #-}

pageSizeL0 :: Integral a => a
pageSizeL0 = factorL0
{-# INLINE pageSizeL0 #-}

mkOpenings :: DVS.Vector Int -> DVS.Vector Word64
mkOpenings u = if DVS.length u == 0
  then DVS.empty
  else DVS.create $ do
    mv <- DVSM.unsafeNew ((DVS.last u + 64 + DVS.length u) `div` 64)
    len <- go 0xffffffffffffffff 0 0 0 0 mv
    return (DVSM.take len mv)
  where go :: Word64 -> Count -> Word64 -> Int -> Int -> DVSM.MVector s Word64 -> ST s Int
        go w wi q ui vi mv = if vi < DVSM.length mv
          then if wi < 64
            then if ui < DVS.length u
              then if p == q
                then go w (wi + 1) q (ui + 1) vi mv
                else if q < p
                  then case p - q of
                    diff -> case min (64 - wi) diff of
                      step -> case step + wi of
                        wj -> case 64 - wj of
                          remainder -> case comp (((((0xffffffffffffffff .<. remainder) .>. remainder)) .>. wi) .<. wi) of
                            mask -> go (w .&. mask) wj (q + step) (ui + 1) vi mv
                  else error "Input data must be monotonically increasing"
              else if wi /= 0
                then go w 64 q ui vi mv
                else return vi
            else do
              DVSM.write mv vi w
              go 0xffffffffffffffff 0 q ui (vi + 1) mv
          else return vi
          where p = fromIntegral (DVS.unsafeIndex u ui) :: Word64

mkJacobson :: DVS.Vector Word64 -> Jacobson
mkJacobson bp = Jacobson
  { jacobsonBP        = bp
  , jacobsonL0Min     = rmL0Min
  , jacobsonL0Excess  = DVS.reword rmL0Excess
  , jacobsonOpenings  = openings
  , jacobsonOpenings2 = mkOpenings openings
  , jacobsonClosings  = closings
  }
  where bpv                   = asVector64 bp
        lenBP                 = fromIntegral (length bpv) :: Int
        lenL0                 = lenBP
        allMinL0              = DVS.generate lenL0 (\i -> if i == lenBP then MinExcess (-64) (-64) else minExcess1 (bpv !!! fromIntegral i))
        rmL0Excess            = DVS.generate lenL0 (\i -> fromIntegral (allExcess1 (DVS.pageFill i pageSizeL0 0xffffffffffffffc0 bpv))) :: DVS.Vector Int16
        rmL0Min               = DVS.generate lenL0 (\i -> let MinExcess minE _ = allMinL0 DVS.! i in fromIntegral minE)
        es                    = fromIntegral <$> DVS.toList rmL0Excess
        ms                    = fromIntegral <$> DVS.toList rmL0Min
        (openings, closings)  = DVS.unzipFromListN2 lenL0 $ builderToIndex (next (lenL0 - 1) (reverse (zip es ms)) [])

type Excess = Int
type Minimum = Int
type Index = Int
type Occurences = Int
type Opening = Int
type Closing = Int

newtype IndexBuilder = IndexBuilder
  { ibOc :: [(Opening, Closing)] -> [(Opening, Closing)]
  }

instance Monoid IndexBuilder where
  mempty = IndexBuilder id

instance Semigroup IndexBuilder where
  a <> b = IndexBuilder
    { ibOc = ibOc a . ibOc b
    }

builderToIndex :: IndexBuilder -> [(Opening, Closing)]
builderToIndex ib = ibOc ib []

ocBuilder :: Int -> Int -> IndexBuilder
ocBuilder o c = IndexBuilder ((o, c):)

next :: Int -> [(Excess, Minimum)] -> [(Index, Occurences)] -> IndexBuilder
next i ((e, m):ems) ps = if m >= 0
    then pop  i e m (e - m) mempty ems ps
    else push i e m                ems ps -- continue
next _ _            _  = mempty

pop :: Int -> Excess -> Minimum -> Opening -> (IndexBuilder) -> [(Excess, Minimum)] -> [(Index, Occurences)] -> IndexBuilder
pop i e m opens accum ems ps = case opens of
  0 -> push i e m ems ps <> accum
  _ -> case ps of
    (q, qc):qs -> case min opens qc of
      stride -> if stride < qc
        then pop i e m (opens - stride) (accum <> ocBuilder i q) ems ((q, stride - qc):qs)
        else pop i e m (opens - stride) (accum <> ocBuilder i q) ems qs
    []         -> mempty

push :: Int -> Excess -> Minimum -> [(Excess, Minimum)] -> [(Index, Occurences)] -> IndexBuilder
push i e m ems ps = if
  | e <  0    -> let closes = -m in next (i - 1) ems ((i, closes):ps)
  | otherwise -> next (i - 1) ems ps

{-
          |  /\      |  /\
          | /  \     | /  \
          |/    \    |/    \
         /|      \  /|      \
/\/\/\/\/ |       \/ |       \/
-}

data FindState = FindBP | FindL0 | FindFromL0

jacobsonFindClose :: Jacobson -> Int -> Count -> Maybe Count
jacobsonFindClose jbp s p = if 0 < i && i < vLen
  then case findCloseN (v !!! fromIntegral i) (fromIntegral s) (((p - 1) `mod` bitLen) + 1) of
    Just q  -> Just q
    Nothing -> error "moo"
  else Nothing
  where i       = fromIntegral ((p - 1) `div` bitLen) :: Int
        bitLen  = 64
        vLen    = fromIntegral (DVS.length v) :: Int
        v       = jacobsonBP jbp

-- if v `newCloseAt` p
--   then if s <= 1
--     then Just p
--     else jacobsonFindCloseL0 v (s - 1) (p + 1)
--   else jacobsonFindCloseL0 v (s + 1) (p + 1)

-- jacobsonFindCloseL0 :: (BitLength a, NewCloseAt a) => Jacobson a -> Int -> Count -> Maybe Count
-- jacobsonFindCloseL0 v s p =
--   let i = p `div` 64 in
--   let mins = jacobsonL0Min v in
--   let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
--   if fromIntegral s + minE <= 0
--     then jacobsonFindClose v s p FindBP
--     else if v `newCloseAt` p && s <= 1
--       then Just p
--       else  let excesses  = jacobsonL0Excess v in
--             let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
--             jacobsonFindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0

-- jacobsonFindCloseL0
-- jacobsonFindClose v s p FindL0 =
--   let i = p `div` 64 in
--   let mins = jacobsonL0Min v in
--   let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
--   if fromIntegral s + minE <= 0
--     then jacobsonFindClose v s p FindBP
--     else if v `newCloseAt` p && s <= 1
--       then Just p
--       else  let excesses  = jacobsonL0Excess v in
--             let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
--             jacobsonFindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
-- jacobsonFindClose v s p FindFromL0
--   | 0 <= p && p < bitLength v   = jacobsonFindClose v s p FindBP
--   | otherwise                   = Nothing
{-# INLINE jacobsonFindClose #-}

instance TestBit Jacobson where
  (.?.) = (.?.) . jacobsonBP
  {-# INLINE (.?.) #-}

instance Rank1 Jacobson where
  rank1 = rank1 . jacobsonBP
  {-# INLINE rank1 #-}

instance Rank0 Jacobson where
  rank0 = rank0 . jacobsonBP
  {-# INLINE rank0 #-}

instance BitLength Jacobson where
  bitLength = bitLength . jacobsonBP
  {-# INLINE bitLength #-}

instance OpenAt Jacobson where
  openAt = openAt . jacobsonBP
  {-# INLINE openAt #-}

instance CloseAt Jacobson where
  closeAt = closeAt . jacobsonBP
  {-# INLINE closeAt #-}

instance NewCloseAt Jacobson where
  newCloseAt = newCloseAt . jacobsonBP
  {-# INLINE newCloseAt #-}

instance FindOpenN Jacobson where
  findOpenN = findOpenN . jacobsonBP
  {-# INLINE findOpenN #-}

instance FindCloseN Jacobson where
  findCloseN v s p  = (+ 1) `fmap` jacobsonFindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN  #-}

instance FindClose Jacobson where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindOpen Jacobson where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v 0 (p - 1)
  {-# INLINE findOpen #-}

instance Enclose Jacobson where
  enclose v = findOpenN v 1
  {-# INLINE enclose #-}

instance BalancedParens Jacobson
