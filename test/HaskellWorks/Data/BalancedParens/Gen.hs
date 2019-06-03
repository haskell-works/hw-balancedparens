{-# LANGUAGE TupleSections #-}

module HaskellWorks.Data.BalancedParens.Gen
  ( BP(..)
  , count
  , balancedParens
  , showBps
  ) where

import Data.Coerce
import Data.Semigroup                ((<>))
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified Hedgehog.Gen as G

count :: MonadGen m => Range Count -> m Count
count r = coerce <$> G.word64 (coerce <$> r)

data LR a = L a Int | R a Int deriving (Eq, Show)

newtype BP = BP [Bool] deriving Eq

showBps :: [Bool] -> String
showBps = fmap fromBool
  where fromBool True  = '('
        fromBool False = ')'

balancedParens' :: MonadGen m => Int -> (Int, [Bool], [Bool], Int) -> m [Bool]
balancedParens' n (ln, lt, rt, rn) = if n <= 0
  then return (reverse lt <> rt)
  else if ln - rn >= n
    then return (reverse lt <> replicate n False <> rt)
    else if rn - ln >= n
      then return (reverse lt <> replicate n True <> rt)
      else do
        decision <- case (ln, rn) of
          (0, 0) -> G.element [L '(' 1,                         R ')' 1]
          (0, _) -> G.element [L '(' 1,             R '(' (-1), R ')' 1]
          (_, 0) -> G.element [L '(' 1, L ')' (-1),             R ')' 1]
          _      -> G.element [L '(' 1,                         R ')' 1]

        case decision of
          L p d -> balancedParens' (n - 1) (ln + d, toBool p:lt,          rt, rn    )
          R p d -> balancedParens' (n - 1) (ln    ,          lt, toBool p:rt, rn + d)
  where toBool '(' = True
        toBool  _  = False

balancedParens ::  MonadGen m => Range Int -> m [Bool]
balancedParens r = do
  n <- G.int r
  balancedParens' (n * 2) (0, [], [], 0)
