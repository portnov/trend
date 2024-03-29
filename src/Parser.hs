
module Parser
--   (parseColumns)
  where

import Text.ParserCombinators.Parsec
import Data.Dates
import System.IO.Unsafe

import Types

pSign :: (Num a) => Parser a
pSign = do
  s <- optionMaybe $ oneOf "+-"
  return $ case s of
             Just '+' -> 1
             Just '-' -> -1
             Nothing -> 1

pNumber :: Parser AnyNumber
pNumber = do
  sgn <- pSign
  m <- pMantiss
  e <- optionMaybe $ oneOf "eE"
  osgn <- pSign
  o <- if e == Nothing
        then return "0"
        else many1 digit
  return $ sgn * m * 10^(osgn*(read o::Int))

pMantiss :: Parser AnyNumber
pMantiss = do
  i <- many digit
  p <- optionMaybe $ oneOf ".,"
  m <- if p == Nothing
        then return "0"
        else if (null i)
                then many1 digit
                else many digit
  let n = length m
  return $ (readAnyNumber i) + (readAnyNumber m)/(10^n)

pAnyNumber :: Parser AnyNumber
pAnyNumber = let now = unsafePerformIO getCurrentDateTime
             in (try $ Date `fmap` pDate now) <|> pNumber

pPair :: Maybe Char -> Parser (AnyNumber, AnyNumber)
pPair mbSep = do
  x <- pAnyNumber
  case mbSep of
    Nothing -> many1 $ oneOf " \t"
    Just sep -> string [sep]
  y <- pAnyNumber
  return (x,y)

pNewline = many1 $ oneOf "\n\r"

pTwoColumns :: Maybe Char -> Parser [(AnyNumber,AnyNumber)]
pTwoColumns mbSep = try (pPair mbSep) `sepEndBy1` pNewline

pOneColumn :: Parser [(AnyNumber,AnyNumber)]
pOneColumn = do
  lst <- pAnyNumber `sepEndBy1` pNewline
  return $ zip (map fromInteger [1..]) lst

pColumns :: Maybe Char ->  Parser [(AnyNumber, AnyNumber)]
pColumns mbSep = do
  x <- choice $ map try [pTwoColumns mbSep, pOneColumn]
  eof
  return x

parseColumns :: Maybe Char -> String -> ([AnyNumber], [AnyNumber])
parseColumns mbSep s =
  let s' = if last s `elem` "\r\n"
             then init s
             else s
  in case parse (pColumns mbSep) "stdin" s' of
      Right d -> unzip d
      Left e -> error $ show e

