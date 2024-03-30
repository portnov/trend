{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Char (toUpper)
import Data.Dates.Formats
import qualified Data.Map as M

import qualified Data.Dates as D

data AnyNumber = 
    Number Double
  | Date D.DateTime
  | Time D.Time
  deriving (Eq,Ord)

instance Show AnyNumber where
  show (Number x) = show x
  show (Date dt) = show dt
  show (Time t) = show t

onAnyNumber :: (Double -> Double -> Double) -> AnyNumber -> AnyNumber -> AnyNumber
onAnyNumber op x y = 
  let t = case x of
            Number _   -> 0
            Date _ -> 1
            Time _ -> 2
  in  fromDouble t $ (toDouble x) `op` (toDouble y)

mapAnyNumber ::  (Double -> Double) -> AnyNumber -> AnyNumber
mapAnyNumber f x =
  let t = case x of
            Number _   -> 0
            Date _ -> 1
            Time _ -> 2
  in  fromDouble t $ f (toDouble x)

instance Num AnyNumber where
  (+) = onAnyNumber (+)
  (-) = onAnyNumber (-)
  (*) = onAnyNumber (*)
  negate = mapAnyNumber negate
  abs = mapAnyNumber abs
  signum = mapAnyNumber signum
  fromInteger n = fromDouble 0 (fromIntegral n)

instance Fractional AnyNumber where
  (/) = onAnyNumber (/)
  recip = mapAnyNumber recip
  fromRational = undefined

instance Floating AnyNumber where
  pi = Number pi
  exp = mapAnyNumber exp
  sqrt = mapAnyNumber sqrt
  log = mapAnyNumber log
  (**) = onAnyNumber (**)
  logBase = onAnyNumber logBase
  sin = mapAnyNumber sin
  cos = mapAnyNumber cos
  asin = mapAnyNumber asin
  atan = mapAnyNumber atan
  acos = mapAnyNumber acos
  sinh = mapAnyNumber sinh
  cosh = mapAnyNumber cosh
  tanh = mapAnyNumber tanh
  asinh = mapAnyNumber asinh
  acosh = mapAnyNumber acosh
  atanh = mapAnyNumber atanh

readAnyNumber :: String -> AnyNumber
readAnyNumber s = Number $ fromIntegral (read s :: Int)

data Mode = Coefs
          | TrendColumn
          | SubTrend
          | Predict {randomize :: !Bool, periods :: !Int}
  deriving (Show)

data Formula = Linear
             | Square
             | Exponent
             | Auto
  deriving (Show)

data ParserSettings =
  ParserSettings {
    psSeparator :: !(Maybe Char),
    psDateFormat :: !(Maybe Format)
  }
  deriving (Show)

data OutputSettings = OutputSettings {
    osStdDev :: Bool
  }
  deriving (Show)

data CmdLine = CmdLine {
    clParser :: !ParserSettings,
    clOutput :: !OutputSettings,
    clByCategory :: !Bool,
    clMode :: !Mode,
    clFormula :: !Formula,
    clInput :: !FilePath
  }
  deriving (Show)

data RegressionResult = RegressionResult {
             xvals :: ![AnyNumber],
             yvals :: ![AnyNumber],
             trend :: ![AnyNumber],
             coefA :: !AnyNumber,
             coefB :: !AnyNumber,
             coefC :: !AnyNumber,
             func :: AnyNumber -> AnyNumber }

data DataSeries = DataSeries {
    dsX :: ![AnyNumber],
    dsY :: ![AnyNumber]
  }
  deriving (Show)

type RegressionInput = M.Map String DataSeries

divD ::  Double -> Double -> Double
a `divD` b = fromIntegral $ floor (a/b)

modD ::  Double -> Double -> Double
a `modD` b = a - b*(a `divD` b)

toDouble :: AnyNumber -> Double
toDouble (Date (D.DateTime y m d hh mm ss)) = 
  let day = fromGregorian (fromIntegral y) m d
      tod = TimeOfDay hh mm (fromIntegral ss)
      locTime = LocalTime day tod
      timestamp = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds $ localTimeToUTC utc locTime
  in fromIntegral $ round timestamp

toDouble (Time (D.Time h m s)) = 60.0*(fromIntegral h)
                      + (fromIntegral m)
                      + (fromIntegral s)/60.0
toDouble (Number d) = d

frac :: Double -> Double
frac x = x - (fromIntegral $ floor x)

fromDouble :: Int -> Double -> AnyNumber
fromDouble 0 x = Number x
fromDouble 1 x =
  let timestamp = secondsToNominalDiffTime $ realToFrac x
      utcTime = posixSecondsToUTCTime timestamp
      locTime = utcToLocalTime utc utcTime
      (y,m,d) = toGregorian $ localDay locTime
      TimeOfDay hh mm ss = localTimeOfDay locTime
  in  Date $ D.DateTime (fromIntegral y) m d
              hh mm (round ss)
fromDouble 2 x = 
  let h = x `divD` 60.0
      ms = x - h*60.0
      m = floor ms
      s = 60.0*frac ms
  in  Time $ D.Time (round h) m (round s)


