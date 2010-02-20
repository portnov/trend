{-# LANGUAGE UnicodeSyntax #-}

module CmdLine where

import Codec.Binary.UTF8.String
import System.Console.GetOpt
import Data.Either

import Types
import Unicode

options ∷ [OptDescr Flag]
options = [
  Option "c" ["coefs"]   (NoArg $ Left Coefs)          "only print coeffs of regression",
  Option "t" ["add"]     (NoArg $ Left TrendColumn)    "add column with trend values",
  Option "s" ["sub"]     (NoArg $ Left SubTrend)       "substract trend from input data",
  Option "p" ["predict"] (OptArg mPeriods "N")         "predict values for N periods",
  Option "r" ["random"]  (OptArg mRandom "N")          "randomize predicted values; implies -p",
  Option "L" ["linear"]  (NoArg $ Right Linear)        "use linear regression (default)",
  Option "S" ["square"]  (NoArg $ Right Square)        "use square regression"
  ]

mPeriods Nothing = Left $ Predict False 1
mPeriods (Just s) = Left $ Predict False (read s)

mRandom Nothing = Left $ Predict False 1
mRandom (Just s) = Left $ Predict True (read s)

parse ∷ [Flag] → Flags
parse flags = F mode formula
  where
    mflags = lefts flags
    fflags = rights flags
    mode | null mflags = Coefs
         | otherwise   = last mflags
    formula | null fflags = Linear
            | otherwise   = last fflags

usage = usageInfo header options
  where
    header = "Usage: trend [OPTIONS...]"

parseCmdLine args = 
  case getOpt RequireOrder options (map decodeString args) of
        (flags, [],      [])     → (parse flags, "")
        (flags, nonOpts, [])     → (parse flags, head nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage
