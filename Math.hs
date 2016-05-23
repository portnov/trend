{-# LANGUAGE PatternGuards #-}

module Math
  (getInfo, predict, subTrend, randoms)
  where

import Control.Monad
import System.Random hiding (randoms)
import Data.List (transpose,minimumBy)
import Data.Function (on)
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms

-- import Data.Random.RVar
-- import Data.Random.Distribution.Normal
-- import Data.Random.Source.DevRandom

import Types

solve :: [[Double]] -> [Double] -> [Double]
solve a b = head $ transpose $ toLists $ linearSolve (fromLists a) (fromLists $ map (:[]) b)

type SystemBuilder = Int -> [AnyNumber] -> [AnyNumber] -> ([[AnyNumber]], [AnyNumber])

systemLinear :: SystemBuilder
systemLinear n xs ys = ([[sx2,sx],[sx,fromIntegral n]], [sxy, sy])
  where
    sx = sum xs
    sx2 = sum $ map (^2) xs
    sy = sum ys
    sxy = sum $ zipWith (*) xs ys

systemSquare :: SystemBuilder
systemSquare n xs ys = ([[sx4, sx3, sx2], [sx3, sx2, sx], [sx2, sx, fromIntegral n]],
                        [sxy2, sxy, sy])
  where
    sx = sum xs
    sy = sum ys
    sx2 = sum $ map (^2) xs
    sx3 = sum $ map (^3) xs
    sx4 = sum $ map (^4) xs
    sxy = sum $ zipWith (*) xs ys
    sxy2 = sum $ zipWith (*) (map (^2) xs) ys

systemExponent :: SystemBuilder
systemExponent n xs ys = systemLinear n xs (map log ys)

mls :: SystemBuilder -> Int -> [AnyNumber] -> [AnyNumber] -> (AnyNumber, AnyNumber,AnyNumber)
mls system n xs ys = 
  let (ma,mb) = system n xs ys
      ans = solve (map (map toDouble) ma) (map toDouble mb)
      (a,b,c) = case ans of
                  (u:v:[]) -> (u,v,0)
                  (u:v:w:[]) -> (u,v,w)
                  _ -> error "Unexpected number of coefficients!"
  in  (Number a,Number b, Number c)

getInfo :: Int -> Formula -> [AnyNumber] -> [AnyNumber] -> Info 
getInfo n f xs ys | Auto <- f = 
  let infoL = getInfo' n Linear xs ys
      infoS = getInfo' n Square xs ys
      infoE = getInfo' n Exponent xs ys
  in  minimumBy (compare `on` stdDev) [infoL, infoS, infoE]
                  | otherwise = getInfo' n f xs ys

getInfo' :: Int -> Formula -> [AnyNumber] -> [AnyNumber] -> Info 
getInfo' n f xs ys = Info xs ys ts a b c formula
  where
    (a,b,c) = mls system n xs ys
    ts      = map formula xs
    (system, formula) =
      case f of
        Linear -> (systemLinear, linear a b)
        Square -> (systemSquare, square a b c)
        Exponent -> (systemExponent, exponential a b)

avgStep :: Info -> AnyNumber
avgStep info = 
  let s = zipWith (-) (tail $ xvals info) (xvals info)
  in  ((sum s)/(fromIntegral $ length s))

stdDev :: Info -> AnyNumber
stdDev info = 
  let ts = trend info
      ys = yvals info
      n = length ts
  in  sqrt $ (sum $ map (^2) $ zipWith (-) ys ts)/(fromIntegral n-1)

-- randomsD :: Double -> Int -> IO [Double]
-- randomsD s n = do
--   replicateM n (runRVar (normal 0 s) DevRandom)
-- 
randoms :: AnyNumber -> Int -> IO [AnyNumber]
randoms s n = do
  lst <- randomsD (toDouble s) n
  return $ map Number lst

randomsD :: Double -> Int -> IO [Double]
randomsD s n = replicateM n $ do
  r1 <- randomIO
  r2 <- randomIO
  return $ s * cos(2*pi*r1)*sqrt(-2*log(r2))

predict :: Int -> Bool -> Info -> IO Info
predict n r info@(Info xs ys ts a b c func) = do
  let st = avgStep info
      xs' = take n $ tail $ iterate (+st) (last xs)
      ys' = map func xs'
  rs <- if r
         then randoms (stdDev info) n
         else return $ replicate n 0
  let ys'' = zipWith (+) ys' rs
  return $ Info (xs++xs') (ys++ys'') (ts++ys') a b c func

subTrend :: Info -> Info
subTrend (Info xs ys ts a b c f) = Info xs ys' ts a b c f
  where
    ys' = zipWith (-) ys ts

linear :: AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber
linear a b x = a*x + b

square :: AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber
square a b c x = a*x^2 + b*x + c

exponential :: AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber
exponential a b x = exp (a*x + b)
