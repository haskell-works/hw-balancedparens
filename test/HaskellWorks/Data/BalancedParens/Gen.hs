{-# LANGUAGE TupleSections #-}

module HaskellWorks.Data.BalancedParens.Gen
  ( BP(..)
  , count
  , roseTree
  , balancedParens
  , showBps
  ) where

import Control.Monad
import Data.Coerce
import Data.List                     (sort)
import Data.Semigroup                ((<>))
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified HaskellWorks.Data.BalancedParens.Internal.RoseTree as RT
import qualified Hedgehog.Gen                                       as G
import qualified Hedgehog.Range                                     as R

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

roseTreeChildren :: MonadGen m => Int -> Int -> m [RT.RoseTree]
roseTreeChildren 0 _ = error "too deep"
roseTreeChildren _ 0 = pure []
roseTreeChildren maxDepth n = case n of
  0 -> pure []
  1 -> pure [RT.RoseTree []]
  _ -> do
    c  <- (n -) <$> G.int (R.linear 0 (n - 1))
    as <- G.list (R.singleton (c - 1)) (G.int (R.linear 0 (n - c)))
    let ns = sort (0:n - c:as)
    let ds = zipWith (-) (drop 1 ns) ns
    forM ds (fmap RT.RoseTree . roseTreeChildren (maxDepth - 1))

roseTree :: MonadGen m => Range Int ->  m RT.RoseTree
roseTree r = do
  n <- G.int r
  fmap RT.RoseTree (roseTreeChildren n (n - 1))
