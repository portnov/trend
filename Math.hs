{-# LANGUAGE UnicodeSyntax #-}

module Math
  (getInfo, predict, subTrend)
  where

import Data.List (transpose)
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms

import Types
import Unicode

solve ∷ [[Double]] → [Double] → [Double]
solve a b = head $ transpose $ toLists $ linearSolve (fromLists a) (fromLists $ map (:[]) b)

system ∷ Int → [AnyNumber] → [AnyNumber] → ([[AnyNumber]], [AnyNumber])
system n xs ys = ([[sx2,sx],[sx,fromIntegral n]], [sxy, sy])
  where
    sx = sum xs
    sx2 = sum $ map (^2) xs
    sy = sum ys
    sxy = sum $ zipWith (*) xs ys

mls ∷ Int → [AnyNumber] → [AnyNumber] → (AnyNumber, AnyNumber)
mls n xs ys = 
  let (ma,mb) = system n xs ys
      (a:b:_) = solve (map (map toDouble) ma) (map toDouble mb)
  in  (Number a,Number b)

getInfo ∷ Int → [AnyNumber] → [AnyNumber] → Info 
getInfo n xs ys = Info xs ys ts a b 
  where
    (a,b) = mls n xs ys
    ts    = map (linear a b) xs

avgStep ∷ Info → AnyNumber
avgStep info = 
  let s = zipWith (-) (tail $ xvals info) (xvals info)
  in  ((sum s)/(fromIntegral $ length s))

predict ∷ Int → Info → Info
predict n info@(Info xs ys ts a b) = 
  let st = avgStep info
      xs' = take n $ tail $ iterate (+st) (last xs)
      ys' = map (linear a b) xs'
  in  Info (xs⧺xs') (ys⧺ys') (ts⧺ys') a b

subTrend ∷ Info → Info
subTrend (Info xs ys ts a b) = Info xs ys' ts a b
  where
    ys' = zipWith (-) ys ts

linear ∷ AnyNumber → AnyNumber → AnyNumber → AnyNumber
linear a b x = a*x + b
