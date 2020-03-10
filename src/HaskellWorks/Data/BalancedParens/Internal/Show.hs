module HaskellWorks.Data.BalancedParens.Internal.Show
  ( showPadded
  ) where

showPadded :: Show a => Int -> a -> String
showPadded n a = reverse (take n (reverse (show a) ++ [' ', ' ' ..]))
