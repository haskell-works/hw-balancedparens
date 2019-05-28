module HaskellWorks.Data.BalancedParens.Gen
  ( count
  ) where

import Data.Coerce
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified Hedgehog.Gen as G

count :: MonadGen m => Range Count -> m Count
count r = coerce <$> G.word64 (coerce <$> r)
