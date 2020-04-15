{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.ParensToBits
  ( cmdParensToBits
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type as Z
import qualified App.IO                    as IO
import qualified Data.ByteString.Lazy      as LBS

{-# ANN module ("HLint: ignore Redundant do"      :: String) #-}
{-# ANN module ("HLint: ignore Redundant return"  :: String) #-}

unparens :: LBS.ByteString -> LBS.ByteString
unparens = LBS.unfoldr go . (0, 0, )
  where go :: (Word8, Count, LBS.ByteString) -> Maybe (Word8, (Word8, Count, LBS.ByteString))
        go (w, c, lbs) = case LBS.uncons lbs of
          Nothing -> if c > 0
            then Just (w, (0, 0, LBS.empty))
            else Nothing
          Just (a, as) -> case a of
            40 -> if c < 8
              then go (w .|. (1 .<. c), c + 1, as)
              else Just (w, (1, 1, as))
            41 -> if c < 8
              then go (w, c + 1, as)
              else Just (w, (0, 1, as))
            _ -> go (w, c, as)

runParensToBits :: Z.ParensToBitsOptions -> IO ()
runParensToBits opts = do
  let inputFile   = opts ^. the @"inputFile"
  let outputFile  = opts ^. the @"outputFile"

  lbs <- IO.readInputFile inputFile

  IO.writeOutputFile outputFile (unparens lbs)

optsParensToBits :: Parser Z.ParensToBitsOptions
optsParensToBits = Z.ParensToBitsOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      <>  value "-"
      )
  <*> strOption
      (   long "output"
      <>  help "Output file"
      <>  metavar "FILE"
      <>  value "-"
      )

cmdParensToBits :: Mod CommandFields (IO ())
cmdParensToBits = command "parens-to-bits"  $ flip info idm $ runParensToBits <$> optsParensToBits
