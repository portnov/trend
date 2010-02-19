{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

module Types where

import Data.Data
import Data.Generics
import Data.Char (toUpper)

import Unicode

data DateTime =
  DateTime {
    year ∷ Int,
    month ∷ Int,
    day ∷ Int,
    hour ∷ Int,
    minute ∷ Int,
    second ∷ Int }
  deriving (Eq,Ord,Data,Typeable)

instance Show DateTime where
  show (DateTime y m d h min s) = 
    show d ⧺ " " ⧺ showMonth m ⧺ " " ⧺ show y ⧺ ", " ⧺
      show h ⧺ ":" ⧺ show min ⧺ ":" ⧺ show s

data Time = 
  Time {
    tHour ∷ Int,
    tMinute ∷ Int,
    tSecond ∷ Int }
  deriving (Eq,Ord,Show,Data,Typeable)

data Mode = Coefs
          | TrendColumn
          | SubTrend
          | Predict {randomize ∷ Bool, periods ∷ Int}

data Info = Info {
             xvals ∷ [Double],
             yvals ∷ [Double],
             trend ∷ [Double],
             coefA ∷ Double,
             coefB ∷ Double }

class ToDouble a where
  toDouble ∷ a → Double

instance ToDouble Double where
  toDouble = id

instance ToDouble DateTime where
  toDouble = dateToDouble 

months ∷ [String]
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]

capitalize ∷ String → String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

showMonth ∷  Int → String
showMonth i = capitalize $ months !! (i-1)

-- a `divD` b = fromIntegral $ round (a/b)
-- a `modD` b = a - b*(a `divD` b)

dateToDouble ∷ DateTime → Double
dateToDouble (DateTime y m d h mi s) = 365.25*(fromIntegral y)
                                     + 30.438*(fromIntegral m)
                                     + (fromIntegral d)
                                     + (fromIntegral h)/24.0
                                     + (fromIntegral mi)/1440.0
                                     + (fromIntegral s) / 86400.0

