{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Time.Calendar
import Data.Char (toUpper)

import qualified Data.Dates as D

data AnyNumber = 
    Number Double
  | Date D.DateTime
  | Time D.Time
  deriving (Eq,Ord)

instance Show AnyNumber where
  show (Number x) = show x
  show (Date (D.DateTime y m d _ _ _)) = show y ++ "/" ++ show m ++ "/" ++ show d
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
          | Predict {randomize :: Bool, periods :: Int}

data Formula = Linear
             | Square
             | Exponent
             | Auto

data CmdLine = CmdLine (Maybe Char) Mode Formula FilePath

data RegressionResult = RegressionResult {
             xvals :: [AnyNumber],
             yvals :: [AnyNumber],
             trend :: [AnyNumber],
             coefA :: AnyNumber,
             coefB :: AnyNumber,
             coefC :: AnyNumber,
             func :: AnyNumber -> AnyNumber }

divD ::  Double -> Double -> Double
a `divD` b = fromIntegral $ floor (a/b)

modD ::  Double -> Double -> Double
a `modD` b = a - b*(a `divD` b)

toDouble :: AnyNumber -> Double
toDouble (Date (D.DateTime y m d _ _ _)) = 
  let day = fromGregorian (fromIntegral y) m d
  in  fromIntegral $ toModifiedJulianDay day

toDouble (Time (D.Time h m s)) = 60.0*(fromIntegral h)
                      + (fromIntegral m)
                      + (fromIntegral s)/60.0
toDouble (Number d) = d

frac :: Double -> Double
frac x = x - (fromIntegral $ floor x)

fromDouble :: Int -> Double -> AnyNumber
fromDouble 0 x = Number x
fromDouble 1 x =
  let (y,m,d) = toGregorian $ ModifiedJulianDay (round x)
  in  Date $ D.DateTime (fromIntegral y) m d 0 0 0
fromDouble 2 x = 
  let h = x `divD` 60.0
      ms = x - h*60.0
      m = floor ms
      s = 60.0*frac ms
  in  Time $ D.Time (round h) m (round s)


