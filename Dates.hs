{-# LANGUAGE DeriveDataTypeable #-}
-- | Operations with dates
module Dates
  (parseDate, pDateTime,
   toDouble)
  where

import Data.Char (toUpper)
import Data.Function (on)
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec

import Types

times :: Int -> Parser t -> Parser [t]
times 0 _ = return []
times n p = do
  ts <- times (n-1) p
  t <- optionMaybe p
  case t of
    Just t' -> return (ts ++ [t'])
    Nothing -> return ts
                               
number :: Int -> Int -> Parser Int
number n m = do
  t <- read `fmap` (n `times` digit)
  if (t > m) || (t < 1)
    then fail "number too large"
    else return t

pYear :: Parser Int
pYear = do
  y <- number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth :: Parser Int
pMonth = number 2 12

pDay :: Parser Int
pDay = number 2 31

euroDate :: Parser AnyNumber
euroDate = do
  d <- pDay
  char '.'
  m <- pMonth
  char '.'
  y <- pYear
  return $ Date y m d

americanDate :: Parser AnyNumber
americanDate = do
  y <- pYear
  char '/'
  m <- pMonth
  char '/'
  d <- pDay
  return $ Date y m d

euroDate' :: Int -> Parser AnyNumber
euroDate' year = do
  d <- pDay
  char '.'
  m <- pMonth
  return $ Date year m d

americanDate' :: Int -> Parser AnyNumber
americanDate' year = do
  m <- pMonth
  char '/'
  d <- pDay
  return $ Date year m d

time24 :: Parser AnyNumber
time24 = do
  h <- number 2 23
  char ':'
  m <- number 2 59
  x <- optionMaybe $ char ':'
  case x of
    Nothing -> return $ Time h m 0
    Just _ -> do
      s <- number 2 59
      notFollowedBy letter
      return $ Time h m s

ampm :: Parser Int
ampm = do
  s <- many1 letter
  case map toUpper s of
    "AM" -> return 0
    "PM" -> return 12
    _ -> fail "AM/PM expected"

time12 :: Parser AnyNumber
time12 = do
  h <- number 2 12
  char ':'
  m <- number 2 59
  x <- optionMaybe $ char ':'
  s <- case x of
            Nothing -> return 0
            Just s' -> number 2 59
  optional space
  hd <- ampm
  return $ Time (h+hd) m s

pDateTime :: Int -> Parser AnyNumber
pDateTime year = choice $ map try $ [
                              time12,
                              time24,
                              euroDate,
                              americanDate,
                              euroDate' year,
                              americanDate' year ]

-- | Parse date/time
parseDate :: Int       -- ^ Current year
          -> String    -- ^ String to parse
          -> Either ParseError AnyNumber
parseDate year s = parse (pDateTime year) "" s

