
module CmdLine where

import Options.Applicative
import Codec.Binary.UTF8.String
import System.Console.GetOpt
import Data.Either

import Types

cmdline :: Parser CmdLine
cmdline = CmdLine <$> mode <*> formula <*> file
  where
    mode =
          flag' Coefs
            ( short 'c'
              <> long "coeffs"
              <> help "only print coeffs of regression (default)"
            )
      <|> flag' TrendColumn
            ( short 't'
              <> long "add"
              <> help "add column with trend values"
            )
      <|> flag' SubTrend
            ( short 's'
              <> long "sub"
              <> help "substract trend from input data"
            )
      <|> predict
      <|> pure Coefs

    predict = Predict
      <$> switch
            ( short 'r'
              <> long "random"
              <> help "randomize predicted values; requires -p"
            )
      <*> option auto
          ( short 'p'
            <> long "predict"
            <> help "predict values for N periods"
            <> metavar "N"
          )

    formula =
          flag' Linear
            ( short 'L'
              <> long "linear"
              <> help "use linear regression (default)"
            )
      <|> flag' Square
            ( short 'S'
              <> long "square"
              <> help "use square regression"
            )
      <|> flag' Exponent
            ( short 'E'
              <> long "exponent"
              <> help "use exponential regression"
            )
      <|> flag' Auto
            ( short 'A'
              <> long "auto"
              <> help "try to guess type of regression"
            )
      <|> pure Linear
    
    file = strArgument (metavar "FILE" <> help "Path to data file or `-' for stdin" <> value "-")

parserInfo :: ParserInfo CmdLine
parserInfo = info (cmdline <**> helper)
  ( fullDesc
    <> progDesc "Command-line tool for regressions calculation"
    <> header "Calculate linear, square or exponential regressions"
  )
