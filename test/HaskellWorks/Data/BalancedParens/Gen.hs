module HaskellWorks.Data.BalancedParens.Gen
  ( count
  , roseTree
  ) where

import Control.Monad
import Data.Coerce
import Data.List                     (sort)
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified HaskellWorks.Data.BalancedParens.Internal.RoseTree as RT
import qualified Hedgehog.Gen                                       as G
import qualified Hedgehog.Range                                     as R

count :: MonadGen m => Range Count -> m Count
count r = coerce <$> G.word64 (coerce <$> r)

roseTreeChildren :: MonadGen m => Int -> Int -> m [RT.RoseTree]
roseTreeChildren 0 _ = error "too deep"
roseTreeChildren _ 0 = pure []
roseTreeChildren maxDepth n = case n of
  0 -> pure []
  1 -> pure [RT.RoseTree []]
  _ -> do
    c  <- G.int (R.constant 1 n)
    as <- G.list (R.singleton (c - 1)) (G.int (R.constant 0 (n - c)))
    let ns = sort (0:n - c:as)
    let ds = zipWith (-) (drop 1 ns) ns
    forM ds (fmap RT.RoseTree . roseTreeChildren (maxDepth - 1))

roseTree :: MonadGen m => Range Int ->  m RT.RoseTree
roseTree r = do
  n <- G.int r
  fmap RT.RoseTree (roseTreeChildren n (n - 1))
