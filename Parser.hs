{-# LANGUAGE UnicodeSyntax #-}

module Parser
--   (parseColumns)
  where

import Text.ParserCombinators.Parsec

import Types
import Unicode
import Dates

pSign ∷ (Num a) ⇒ Parser a
pSign = do
  s ← optionMaybe $ oneOf "+-"
  return $ case s of
             Just '+' → 1
             Just '-' → -1
             Nothing → 1

pNumber ∷ Parser AnyNumber
pNumber = do
  sgn ← pSign
  m ← pMantiss
  e ← optionMaybe $ oneOf "eE"
  osgn ← pSign
  o ← if e == Nothing
        then return "0"
        else many1 digit
  return $ sgn * m * 10^(osgn*(read o∷Int))

pMantiss ∷ Parser AnyNumber
pMantiss = do
  i ← many digit
  p ← optionMaybe $ oneOf ".,"
  m ← if p == Nothing
        then return "0"
        else if (null i)
                then many1 digit
                else many digit
  let n = length m
  return $ (readAnyNumber i) + (readAnyNumber m)/(10^n)

pAnyNumber ∷ Parser AnyNumber
pAnyNumber = (try $ pDateTime 2010) <|> pNumber

pPair ∷ Parser (AnyNumber, AnyNumber)
pPair = do
  x ← pAnyNumber
  many1 $ oneOf " \t"
  y ← pAnyNumber
  return (x,y)

pNewline = many1 $ oneOf "\n\r"

pTwoColumns ∷ Parser [(AnyNumber,AnyNumber)]
pTwoColumns = try pPair `sepEndBy` pNewline

pOneColumn ∷ Parser [(AnyNumber,AnyNumber)]
pOneColumn = do
  lst ← pAnyNumber `sepEndBy` pNewline
  return $ zip (map fromInteger [1..]) lst

pColumns ∷  Parser [(AnyNumber, AnyNumber)]
pColumns = do
  x ← choice $ map try [pTwoColumns, pOneColumn]
  eof
  return x

parseColumns ∷  String → ([AnyNumber], [AnyNumber])
parseColumns s =
  let s' = if last s ∈ "\r\n"
             then init s
             else s
  in case parse pColumns "stdin" s' of
      Right d → unzip d
      Left e → error $ show e
