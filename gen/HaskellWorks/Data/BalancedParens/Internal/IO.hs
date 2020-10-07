module HaskellWorks.Data.BalancedParens.Internal.IO
  ( safeListDirectory
  ) where

import qualified System.Directory as IO

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory fp = do
  exists <- IO.doesDirectoryExist fp
  if exists
    then IO.listDirectory fp
    else return []
