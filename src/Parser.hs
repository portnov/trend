
module Parser
--   (parseColumns)
  where

import Text.ParserCombinators.Parsec
import Data.Dates
import System.IO.Unsafe
import qualified Data.Map as M

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

pPair :: Maybe Char -> Parser a -> Parser (a, AnyNumber)
pPair mbSep pFirst = do
  x <- pFirst
  case mbSep of
    Nothing -> many1 $ oneOf " \t"
    Just sep -> string [sep]
  y <- pAnyNumber
  return (x,y)

pTriplet :: Maybe Char -> Parser (String, AnyNumber, AnyNumber)
pTriplet mbSep = do
  x <- pAnyNumber
  case mbSep of
    Nothing -> many1 $ oneOf " \t"
    Just sep -> string [sep]
  (category, y) <- pPair mbSep (pCategory mbSep)
  return (category, x, y)

pNewline = many1 $ oneOf "\n\r"

pOneColumn :: Parser [(AnyNumber,AnyNumber)]
pOneColumn = do
  lst <- pAnyNumber `sepEndBy1` pNewline
  return $ zip (map fromInteger [1..]) lst

pTwoColumns :: Maybe Char -> Parser [(AnyNumber,AnyNumber)]
pTwoColumns mbSep = try (pPair mbSep pAnyNumber) `sepEndBy1` pNewline

pCategory :: Maybe Char -> Parser String
pCategory Nothing = many1 $ noneOf " \t"
pCategory (Just sep) = many1 $ noneOf [sep]

pOneColumnWithCategories :: Maybe Char -> Parser [(String, AnyNumber, AnyNumber)]
pOneColumnWithCategories mbSep = do
  lst <- try (pPair mbSep (pCategory mbSep)) `sepEndBy1` pNewline
  let (categories, ys) = unzip lst
  return $ zip3 categories (map fromInteger [1..]) ys

pTwoColumnsWithCategories :: Maybe Char -> Parser [(String, AnyNumber, AnyNumber)]
pTwoColumnsWithCategories mbSep =
  try (pTriplet mbSep) `sepEndBy1` pNewline

pColumns :: Maybe Char ->  Parser [(AnyNumber, AnyNumber)]
pColumns mbSep = do
  x <- choice $ map try [pTwoColumns mbSep, pOneColumn]
  eof
  return x

pColumnsWithCategories :: Maybe Char -> Parser [(String, AnyNumber, AnyNumber)]
pColumnsWithCategories mbSep = do
  x <- choice $ map try [pTwoColumnsWithCategories mbSep, pOneColumnWithCategories mbSep]
  eof
  return x

parseColumns :: Maybe Char -> String -> RegressionInput
parseColumns mbSep s =
  let s' = if last s `elem` "\r\n"
             then init s
             else s
  in case parse (pColumns mbSep) "stdin" s' of
      Right d -> let (xs, ys) = unzip d
                 in  M.singleton "input" $ DataSeries xs ys
      Left e -> error $ show e

parseColumnsWithCategories :: Maybe Char -> String -> RegressionInput
parseColumnsWithCategories mbSep s =
  let s' = if last s `elem` "\r\n"
             then init s
             else s

      preprocess r [] = r
      preprocess r ((category, x, y):rest) =
          M.insertWith append category (DataSeries [x] [y]) $ preprocess r rest

      append (DataSeries xs1 ys1) (DataSeries xs2 ys2) = DataSeries (xs1 ++ xs2) (ys1 ++ ys2)

  in case parse (pColumnsWithCategories mbSep) "stdin" s' of
      Left e -> error $ show e
      Right d -> preprocess M.empty d

