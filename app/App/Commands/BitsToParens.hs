{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.BitsToParens
  ( cmdBitsToParens
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type as Z
import qualified App.IO                    as IO
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as LBS

{-# ANN module ("HLint: ignore Redundant do"      :: String) #-}
{-# ANN module ("HLint: ignore Redundant return"  :: String) #-}

bitString :: Word8 -> B.Builder
bitString w =
  go ((w .>. 0) .&. 1) <>
  go ((w .>. 1) .&. 1) <>
  go ((w .>. 2) .&. 1) <>
  go ((w .>. 3) .&. 1) <>
  go ((w .>. 4) .&. 1) <>
  go ((w .>. 5) .&. 1) <>
  go ((w .>. 6) .&. 1) <>
  go ((w .>. 7) .&. 1)
  where go :: Word8 -> B.Builder
        go 1 = B.word8 40
        go _ = B.word8 41

parensBuilder :: LBS.ByteString -> B.Builder
parensBuilder lbs = case LBS.uncons lbs of
  Just (w, rs) -> bitString w <> parensBuilder rs
  Nothing      -> mempty

parens :: LBS.ByteString -> LBS.ByteString
parens = B.toLazyByteString . parensBuilder

runBitsToParens :: Z.BitsToParensOptions -> IO ()
runBitsToParens opts = do
  let inputFile   = opts ^. the @"inputFile"
  let outputFile  = opts ^. the @"outputFile"

  lbs <- IO.readInputFile inputFile

  IO.writeOutputFile outputFile $ parens lbs

  return ()

optsBitsToParens :: Parser Z.BitsToParensOptions
optsBitsToParens = Z.BitsToParensOptions
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

cmdBitsToParens :: Mod CommandFields (IO ())
cmdBitsToParens = command "bits-to-parens"  $ flip info idm $ runBitsToParens <$> optsBitsToParens
