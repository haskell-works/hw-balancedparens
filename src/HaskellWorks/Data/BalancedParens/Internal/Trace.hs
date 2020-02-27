module HaskellWorks.Data.BalancedParens.Internal.Trace
  ( traceW
  ) where

import Data.Semigroup ((<>))
import Debug.Trace

traceW :: Show a => String -> a -> a
traceW s a = trace (s <> " = " <> show a) a
