{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Functor (void)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype Name = Name Text
  deriving Show

newtype Birthday = Birthday Day -- (MonthOfYear, DayOfMonth)
  deriving Show

birthdayParser :: Parser Birthday
birthdayParser = do
  year <- read <$> count 4 digitChar <?> "year"
  optional $ char '-'
  month <- read <$> count 2 digitChar <?> "month"
  optional $ char '-'
  day <- read <$> count 2 digitChar <?> "day"
  pure . Birthday $ fromGregorian year month day

newtype Contact = Contact Name
  deriving Show

vcardParser :: Parser Contact
vcardParser = do
  string "BEGIN:VCARD" *> eol
  keyValues <- someTill contentline (string "END:VCARD" *> eol)
  let (Just fName) = snd <$> find ((== "FN") . fst) keyValues
  pure . Contact . Name $ fName

  where
    contentline :: Parser (Text, Text)
    contentline = do
      name <- name
      char ':'
      value <- value
      eol
      pure (T.pack name, T.pack value)
      -- <?> "contentline"
    name = some $ alphaNumChar <|> anySingleBut ':' -- oneOf ['-', '.', ';', '=']
    value = many printChar

vcardsParser :: Parser [Contact]
vcardsParser = some vcardParser <* eof

someFunc :: IO ()
someFunc = putStrLn "someFunc"
