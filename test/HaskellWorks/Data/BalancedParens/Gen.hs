module HaskellWorks.Data.BalancedParens.Gen
  ( count
  , roseTree
  ) where

import Data.Coerce
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified HaskellWorks.Data.BalancedParens.Internal.RoseTree as RT
import qualified Hedgehog.Gen                                       as G

count :: MonadGen m => Range Count -> m Count
count r = coerce <$> G.word64 (coerce <$> r)

roseTree :: MonadGen m => Range Int -> Range Int ->  m RT.RoseTree
roseTree = go 0
  where go :: MonadGen m => Int -> Range Int -> Range Int ->  m RT.RoseTree
        go height rangeHeight rangeBranchiness = do
          gh <- G.int rangeHeight
          if gh > height
            then do
              children <- G.list rangeBranchiness (go (height + 1) rangeHeight rangeBranchiness)
              return (RT.RoseTree children)
            else return (RT.RoseTree [])
