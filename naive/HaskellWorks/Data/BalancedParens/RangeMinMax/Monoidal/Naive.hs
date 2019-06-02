{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.BalancedParens.RangeMinMax.Monoidal.Naive
  ( T.RmmEx(..)
  , fromBools
  ) where

import HaskellWorks.Data.FingerTree ((|>))

import qualified HaskellWorks.Data.BalancedParens.RangeMinMax.Monoidal.Naive.Type as T
import qualified HaskellWorks.Data.FingerTree                                     as FT

fromBools :: [Bool] -> T.RmmEx
fromBools cs = T.RmmEx (foldl go FT.empty cs)
  where go :: FT.FingerTree T.Size (T.Elem Bool) -> Bool -> FT.FingerTree T.Size (T.Elem Bool)
        go ft e = ft |> T.Elem e
