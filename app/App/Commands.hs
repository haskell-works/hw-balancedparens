module App.Commands where

import App.Commands.BitsToParens
import App.Commands.ParensToBits
import App.Commands.Positions
import Options.Applicative

{- HLINT ignore "Monoid law, left identity" -}

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdParensToBits
  <>  cmdBitsToParens
  <>  cmdPositions
