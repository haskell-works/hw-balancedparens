{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}

module HaskellWorks.Data.BalancedParens.Internal.ParensSeq
  ( Elem(..)
  , Measure(..)
  , ParensSeq(..)
  , ParensSeqFt
  , (|>#)
  , (#<|)
  , ftSplit
  , atSizeBelowZero
  , atMinZero
  ) where

import Control.DeepSeq
import Data.Int
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.MinExcess
import HaskellWorks.Data.Excess.PartialMinExcess1
import HaskellWorks.Data.FingerTree               (ViewL (..), ViewR (..), (<|), (><), (|>))
import HaskellWorks.Data.Positioning
import Prelude                                    hiding (max, min)

import qualified HaskellWorks.Data.Cons       as HW
import qualified HaskellWorks.Data.Container  as HW
import qualified HaskellWorks.Data.FingerTree as FT
import qualified HaskellWorks.Data.Snoc       as HW
import qualified Prelude                      as P

data Elem = Elem
  { bps  :: {-# UNPACK #-} !Word64
  , size :: {-# UNPACK #-} !Count
  } deriving (Eq, Show, Generic)

instance NFData Elem

data Measure = Measure
  { size   :: {-# UNPACK #-} !Count
  , min    :: {-# UNPACK #-} !Int
  , excess :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord, Show, Generic)

instance NFData Measure

type ParensSeqFt = FT.FingerTree Measure Elem

newtype ParensSeq = ParensSeq
  { parens :: ParensSeqFt
  } deriving (Show, NFData, Generic)

instance Semigroup Measure where
  Measure aSize aMin aExcess <> Measure bSize bMin bExcess = Measure
    { size    = aSize + bSize
    , min     = P.min aMin (bMin + aExcess)
    , excess  = aExcess + bExcess
    }

instance Monoid Measure where
  mempty = Measure 0 0 0
#if MIN_VERSION_GLASGOW_HASKELL(8, 4, 4, 0)
#else
  mappend = (<>)
#endif

instance FT.Measured Measure Elem where
  measure (Elem w size) = Measure { min, excess, size }
    where MinExcess min excess = partialMinExcess1 (fromIntegral size) w

instance HW.Container ParensSeq where
  type Elem ParensSeq = Bool

instance HW.Cons ParensSeq where
  cons b (ParensSeq ft) = ParensSeq $ case FT.viewl ft of
    Elem w nw :< rt -> if nw >= 0 && nw < 64
      then Elem ((w .<. 1) .|. bw) (nw + 1) <| rt
      else Elem bw 1                        <| ft
    FT.EmptyL        -> FT.singleton (Elem bw 1)
    where bw = if b then 1 else 0

instance HW.Snoc ParensSeq where
  snoc (ParensSeq ft) b = ParensSeq $ case FT.viewr ft of
    lt :> Elem w nw -> if nw >= 0 && nw < 64
      then Elem (w .|. (bw .<. nw)) (nw + 1) <| lt
      else Elem bw 1                         <| lt
    FT.EmptyR        -> FT.singleton (Elem bw 1)
    where bw = if b then 1 else 0

instance Semigroup ParensSeq where
  ParensSeq tl <> ParensSeq tr = ParensSeq $ case FT.viewr tl of
    tll :> Elem wl nwl -> case FT.viewl tr of
      Elem wr nwr :< trr -> let nw = nwl + nwr in if nw <= 64
        then (tll |> Elem (wl .|. (wr .<. nwl)) nw) >< trr
        else tl >< tr
      FT.EmptyL -> tr
    FT.EmptyR -> FT.empty

(|>#) :: ParensSeqFt -> Elem -> ParensSeqFt
(|>#) ft e@(Elem _ wn) = if wn > 0 then ft |> e else ft

(#<|) :: Elem ->ParensSeqFt -> ParensSeqFt
(#<|) e@(Elem _ wn) ft = if wn > 0 then e <| ft else ft

ftSplit :: (Measure -> Bool) -> ParensSeqFt -> (ParensSeqFt, ParensSeqFt)
ftSplit p ft = case FT.viewl rt of
  Elem w nw :< rrt -> let c = go w nw nw in (lt |># Elem w c, Elem (w .>. c) (nw - c) #<| rrt)
  FT.EmptyL        -> (ft, FT.empty)
  where (lt, rt) = FT.split p ft
        ltm = FT.measure lt
        go :: Word64 -> Count -> Count -> Count
        go w c nw = if c > 0
          then if p (ltm <> FT.measure (Elem (w .<. (64 - c) .>. (64 - c)) c))
            then go w (c - 1) nw
            else c
          else 0

atSizeBelowZero :: Count -> Measure -> Bool
atSizeBelowZero n (Measure { size = sz }) = n < sz

atMinZero :: Measure -> Bool
atMinZero (Measure { min = m }) = m <= 0
