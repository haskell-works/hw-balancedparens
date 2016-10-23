{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.BalancedParens.Simple
  ( SimpleBalancedParens(..)
  ) where

import           Control.Monad
import           HaskellWorks.Data.BalancedParens.BalancedParens
import           HaskellWorks.Data.BalancedParens.CloseAt
import           HaskellWorks.Data.BalancedParens.Enclose
import           HaskellWorks.Data.BalancedParens.FindClose
import           HaskellWorks.Data.BalancedParens.FindOpen
import           HaskellWorks.Data.BalancedParens.OpenAt
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.RankSelect.Base.Select0
import           HaskellWorks.Data.RankSelect.Base.Select1
import           Prelude                                                    as P

newtype SimpleBalancedParens a = SimpleBalancedParens a
  deriving (BalancedParens, FindOpen, FindClose, Enclose, OpenAt, CloseAt, BitLength, BitShow, Eq, Rank0, Rank1, Select0, Select1, TestBit)

instance Functor SimpleBalancedParens where
  fmap f (SimpleBalancedParens a) = SimpleBalancedParens (f a)
  {-# INLINABLE fmap   #-}

instance BitShow a => Show (SimpleBalancedParens a) where
  show = bitShow
