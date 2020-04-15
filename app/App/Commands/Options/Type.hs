{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics

newtype PositionsOptions = PositionsOptions
  { inputFile :: FilePath
  } deriving (Eq, Show, Generic)

data BitsToParensOptions = BitsToParensOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)

data ParensToBitsOptions = ParensToBitsOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)
