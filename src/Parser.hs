
module Parser
--   (parseColumns)
  where

import Text.ParserCombinators.Parsec
import Data.Dates
import Data.Dates.Formats
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

pAnyNumber :: ParserSettings -> Parser AnyNumber
pAnyNumber settings =
  let now = unsafePerformIO getCurrentDateTime
      dateParser = case psDateFormat settings of
                     Nothing -> pDateTime now
                     Just fmt -> formatParser fmt
  in (try $ Date `fmap` dateParser) <|> pNumber

pPair :: ParserSettings -> Parser a -> Parser (a, AnyNumber)
pPair settings pFirst = do
  x <- pFirst
  case psSeparator settings of
    Nothing -> many1 $ oneOf " \t"
    Just sep -> string [sep]
  y <- pAnyNumber settings
  return (x,y)

pTriplet :: ParserSettings -> Parser (String, AnyNumber, AnyNumber)
pTriplet settings = do
  x <- pAnyNumber settings
  case psSeparator settings of
    Nothing -> many1 $ oneOf " \t"
    Just sep -> string [sep]
  (category, y) <- pPair settings (pCategory settings)
  return (category, x, y)

pNewline = many1 $ oneOf "\n\r"

pOneColumn :: ParserSettings -> Parser [(AnyNumber,AnyNumber)]
pOneColumn settings = do
  lst <- pAnyNumber settings `sepEndBy1` pNewline
  return $ zip (map fromInteger [1..]) lst

pTwoColumns :: ParserSettings -> Parser [(AnyNumber,AnyNumber)]
pTwoColumns settings = try (pPair settings (pAnyNumber settings)) `sepEndBy1` pNewline

pCategory :: ParserSettings -> Parser String
pCategory settings =
  case psSeparator settings of
    Nothing -> many1 $ noneOf " \t"
    Just sep -> many1 $ noneOf [sep]

pOneColumnWithCategories :: ParserSettings -> Parser [(String, AnyNumber, AnyNumber)]
pOneColumnWithCategories settings = do
  lst <- try (pPair settings (pCategory settings)) `sepEndBy1` pNewline
  let (categories, ys) = unzip lst
  return $ zip3 categories (map fromInteger [1..]) ys

pTwoColumnsWithCategories :: ParserSettings -> Parser [(String, AnyNumber, AnyNumber)]
pTwoColumnsWithCategories settings =
  try (pTriplet settings) `sepEndBy1` pNewline

pColumns :: ParserSettings ->  Parser [(AnyNumber, AnyNumber)]
pColumns settings = do
  x <- choice $ map try [pTwoColumns settings, pOneColumn settings]
  eof
  return x

pColumnsWithCategories :: ParserSettings -> Parser [(String, AnyNumber, AnyNumber)]
pColumnsWithCategories settings = do
  x <- choice $ map try [pTwoColumnsWithCategories settings, pOneColumnWithCategories settings]
  eof
  return x

parseColumns :: ParserSettings -> String -> RegressionInput
parseColumns settings s =
  let s' = if last s `elem` "\r\n"
             then init s
             else s
  in case parse (pColumns settings) "stdin" s' of
      Right d -> let (xs, ys) = unzip d
                 in  M.singleton "input" $ DataSeries xs ys
      Left e -> error $ show e

parseColumnsWithCategories :: ParserSettings -> String -> RegressionInput
parseColumnsWithCategories settings s =
  let s' = if last s `elem` "\r\n"
             then init s
             else s

      preprocess r [] = r
      preprocess r ((category, x, y):rest) =
          M.insertWith append category (DataSeries [x] [y]) $ preprocess r rest

      append (DataSeries xs1 ys1) (DataSeries xs2 ys2) = DataSeries (xs1 ++ xs2) (ys1 ++ ys2)

  in case parse (pColumnsWithCategories settings) "stdin" s' of
      Left e -> error $ show e
      Right d -> preprocess M.empty d

