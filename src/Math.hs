{-# LANGUAGE PatternGuards #-}

module Math
  (calculateMany, predictMany, subTrendMany, randoms, systemLinear)
  where

import Control.Monad
import Data.Maybe
import System.Random hiding (randoms)
import Data.List (transpose,minimumBy)
import qualified Data.Map as M
import Data.Function (on)
import Numeric.LinearAlgebra

-- import Data.Random.RVar
-- import Data.Random.Distribution.Normal
-- import Data.Random.Source.DevRandom

import Types

solve :: [[Double]] -> [Double] -> [Double]
solve a b = head $ transpose $ toLists $ fromJust $ linearSolve (fromLists a) (fromLists $ map (:[]) b)

type SystemBuilder = Int -> DataSeries -> ([[AnyNumber]], [AnyNumber])

systemLinear :: SystemBuilder
systemLinear n (DataSeries xs ys) = ([[sx2,sx],[sx,fromIntegral n]], [sxy, sy])
  where
    sx = sum xs
    sx2 = sum $ map (^2) xs
    sy = sum ys
    sxy = sum $ zipWith (*) xs ys

systemSquare :: SystemBuilder
systemSquare n (DataSeries xs ys) =
    ([[sx4, sx3, sx2], [sx3, sx2, sx], [sx2, sx, fromIntegral n]],
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
systemExponent n (DataSeries xs ys) = systemLinear n $ DataSeries xs (map log ys)

mls :: SystemBuilder -> Int -> DataSeries -> (AnyNumber, AnyNumber,AnyNumber)
mls system n ds = 
  let (ma,mb) = system n ds
      ans = solve (map (map toDouble) ma) (map toDouble mb)
      (a,b,c) = case ans of
                  (u:v:[]) -> (u,v,0)
                  (u:v:w:[]) -> (u,v,w)
                  _ -> error "Unexpected number of coefficients!"
  in  (Number a,Number b, Number c)

calculateMany :: Formula -> RegressionInput -> M.Map String RegressionResult
calculateMany f input = M.map calculateSeries input
  where
    calculateSeries ds = calculate (length $ dsX ds) f ds

calculate :: Int -> Formula -> DataSeries -> RegressionResult 
calculate n f ds | Auto <- f = 
  let infoL = calculate' n Linear ds
      infoS = calculate' n Square ds
      infoE = calculate' n Exponent ds
  in  minimumBy (compare `on` stdDev) [infoL, infoS, infoE]
                  | otherwise = calculate' n f ds

calculate' :: Int -> Formula -> DataSeries -> RegressionResult 
calculate' n f ds@(DataSeries xs ys) = RegressionResult xs ys ts a b c formula
  where
    (a,b,c) = mls system n ds
    ts      = map formula xs
    (system, formula) =
      case f of
        Linear -> (systemLinear, linear a b)
        Square -> (systemSquare, square a b c)
        Exponent -> (systemExponent, exponential a b)

avgStep :: RegressionResult -> AnyNumber
avgStep info = 
  let s = zipWith (-) (tail $ xvals info) (xvals info)
  in  ((sum s)/(fromIntegral $ length s))

stdDev :: RegressionResult -> AnyNumber
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

predict :: Int -> Bool -> RegressionResult -> IO RegressionResult
predict n r info@(RegressionResult xs ys ts a b c func) = do
  let st = avgStep info
      xs' = take n $ tail $ iterate (+st) (last xs)
      ys' = map func xs'
  rs <- if r
         then randoms (stdDev info) n
         else return $ replicate n 0
  let ys'' = zipWith (+) ys' rs
  return $ RegressionResult (xs++xs') (ys++ys'') (ts++ys') a b c func

predictMany :: Int -> Bool -> M.Map String RegressionResult -> IO (M.Map String RegressionResult)
predictMany n r result = do
  result' <- forM (M.assocs result) $ \(category, info) -> do
               info' <- predict n r info
               return (category, info')
  return $ M.fromList result'

subTrend :: RegressionResult -> RegressionResult
subTrend (RegressionResult xs ys ts a b c f) = RegressionResult xs ys' ts a b c f
  where
    ys' = zipWith (-) ys ts

subTrendMany :: M.Map String RegressionResult -> M.Map String RegressionResult
subTrendMany = M.map subTrend

linear :: AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber
linear a b x = a*x + b

square :: AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber
square a b c x = a*x^2 + b*x + c

exponential :: AnyNumber -> AnyNumber -> AnyNumber -> AnyNumber
exponential a b x = exp (a*x + b)
