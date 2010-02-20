{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

module Math
  (getInfo, predict, subTrend)
  where

import Data.List (transpose,minimumBy)
import Data.Function (on)
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms

import Types
import Unicode

solve ∷ [[Double]] → [Double] → [Double]
solve a b = head $ transpose $ toLists $ linearSolve (fromLists a) (fromLists $ map (:[]) b)

type SystemBuilder = Int → [AnyNumber] → [AnyNumber] → ([[AnyNumber]], [AnyNumber])

systemLinear ∷ SystemBuilder
systemLinear n xs ys = ([[sx2,sx],[sx,fromIntegral n]], [sxy, sy])
  where
    sx = sum xs
    sx2 = sum $ map (^2) xs
    sy = sum ys
    sxy = sum $ zipWith (*) xs ys

systemSquare ∷ SystemBuilder
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

systemExponent ∷ SystemBuilder
systemExponent n xs ys = systemLinear n xs (map log ys)

mls ∷ SystemBuilder → Int → [AnyNumber] → [AnyNumber] → (AnyNumber, AnyNumber,AnyNumber)
mls system n xs ys = 
  let (ma,mb) = system n xs ys
      ans = solve (map (map toDouble) ma) (map toDouble mb)
      (a,b,c) = case ans of
                  (u:v:[]) → (u,v,0)
                  (u:v:w:[]) → (u,v,w)
                  _ → error "Unexpected number of coefficients!"
  in  (Number a,Number b, Number c)

getInfo ∷ Int → Formula → [AnyNumber] → [AnyNumber] → Info 
getInfo n f xs ys | Auto ← f = 
  let infoL = getInfo' n Linear xs ys
      infoS = getInfo' n Square xs ys
      infoE = getInfo' n Exponent xs ys
  in  minimumBy (compare `on` stdDev) [infoL, infoS, infoE]
                  | otherwise = getInfo' n f xs ys

getInfo' ∷ Int → Formula → [AnyNumber] → [AnyNumber] → Info 
getInfo' n f xs ys = Info xs ys ts a b c formula
  where
    (a,b,c) = mls system n xs ys
    ts      = map formula xs
    (system, formula) =
      case f of
        Linear → (systemLinear, linear a b)
        Square → (systemSquare, square a b c)
        Exponent → (systemExponent, exponential a b)

avgStep ∷ Info → AnyNumber
avgStep info = 
  let s = zipWith (-) (tail $ xvals info) (xvals info)
  in  ((sum s)/(fromIntegral $ length s))

stdDev ∷ Info → AnyNumber
stdDev info = 
  let ts = trend info
      ys = yvals info
      n = length ts
  in  (sum $ map (^2) $ zipWith (-) ys ts)/(fromIntegral n-1)

predict ∷ Int → Info → Info
predict n info@(Info xs ys ts a b c func) = 
  let st = avgStep info
      xs' = take n $ tail $ iterate (+st) (last xs)
      ys' = map func xs'
  in  Info (xs⧺xs') (ys⧺ys') (ts⧺ys') a b c func

subTrend ∷ Info → Info
subTrend (Info xs ys ts a b c f) = Info xs ys' ts a b c f
  where
    ys' = zipWith (-) ys ts

linear ∷ AnyNumber → AnyNumber → AnyNumber → AnyNumber
linear a b x = a*x + b

square ∷ AnyNumber → AnyNumber → AnyNumber → AnyNumber → AnyNumber
square a b c x = a*x^2 + b*x + c

exponential ∷ AnyNumber → AnyNumber → AnyNumber → AnyNumber
exponential a b x = exp (a*x + b)
