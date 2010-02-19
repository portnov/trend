{-# LANGUAGE UnicodeSyntax #-}

module CmdLine where

import Codec.Binary.UTF8.String
import System.Console.GetOpt

import Types
import Unicode

options ∷ [OptDescr Mode]
options = [
  Option "c" ["coefs"]   (NoArg Coefs)          "only print coeffs of linear regression",
  Option "t" ["add"]     (NoArg TrendColumn)    "add column with trend values",
  Option "s" ["sub"]     (NoArg SubTrend)       "substract trend from input data",
  Option "p" ["predict"] (OptArg mPeriods "N")  "predict values for N periods",
  Option "r" ["random"]  (OptArg mRandom "N")   "randomize predicted values; implies -p"
  ]

mPeriods Nothing = Predict False 1
mPeriods (Just s) = Predict False (read s)

mRandom Nothing = Predict False 1
mRandom (Just s) = Predict True (read s)

usage = usageInfo header options
  where
    header = "Usage: trend [OPTIONS...]"

parseCmdLine args = 
  case getOpt RequireOrder options (map decodeString args) of
        (flags, [],      [])     → (last flags, "")
        (flags, nonOpts, [])     → (last flags, head nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage
