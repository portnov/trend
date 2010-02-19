{-# LANGUAGE UnicodeSyntax #-}

module Parser
--   (parseColumns)
  where

import Text.ParserCombinators.Parsec

import Unicode
import Dates

pSign ∷ (Num a) => Parser a
pSign = do
  s ← optionMaybe $ oneOf "+-"
  return $ case s of
             Just '+' → 1
             Just '-' → -1
             Nothing → 1

pNumber ∷ Parser Double
pNumber = do
  sgn ← pSign
  m ← pMantiss
  e ← optionMaybe $ oneOf "eE"
  osgn ← pSign
  o ← if e == Nothing
        then return "0"
        else many1 digit
  return $ sgn * m * 10^(osgn*(read o∷Int))

pMantiss ∷ Parser Double
pMantiss = do
  i ← many digit
  p ← optionMaybe $ oneOf ".,"
  m ← if p == Nothing
        then return "0"
        else if (null i)
                then many1 digit
                else many digit
  let n = length m
  return $ (read i) + (read m)/(10^n)

pDoubleDate ∷ Parser Double
pDoubleDate = do
  dt ← pAbsDate 2010
  char '\t'
  return $ toDouble dt

pNumberOrDate ∷ Parser Double
pNumberOrDate = (try pNumber) <|> pDoubleDate 

pPair ∷ Parser (Double, Double)
pPair = do
  x ← pNumber
  many1 $ oneOf " \t"
  y ← pNumber
  return (x,y)

pNewline = many1 $ oneOf "\n\r"

pTwoColumns ∷ Parser [(Double,Double)]
pTwoColumns = try pPair `sepEndBy` pNewline

pOneColumn ∷ Parser [(Double,Double)]
pOneColumn = do
  lst ← pNumber `sepEndBy` pNewline
  return $ zip [1..] lst

pColumns ∷  Parser [(Double, Double)]
pColumns = do
  x ← choice $ map try [pOneColumn, pTwoColumns]
  eof
  return x

parseColumns ∷  String → ([Double], [Double])
parseColumns s =
  let s' = if last s ∈ "\r\n"
             then init s
             else s
  in case parse pColumns "stdin" s' of
      Right d → unzip d
      Left e → error $ show e
