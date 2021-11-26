{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad.Trans.Maybe
import Data.Functor (void)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

-- https://github.com/haskell/time/commit/c69d1bd9b06d809f69fec8504896d0834d91476e
-- #if !MIN_VERSION_time(1,11,0)
-- `#if` doesn't work: `error on input ‘#’`
type Year = Integer
type MonthOfYear = Int
type DayOfMonth = Int
-- #endif

type Parser = Parsec Void Text

newtype Name = Name Text
  deriving Show

data Birthday
  = Full Day
  | Partial MonthOfYear DayOfMonth
  deriving Show

-- If a year is passed, it should be parsed and will then be ignored because
-- it's known to be a placeholder. The vCard property looks like this:
-- `BDAY;X-APPLE-OMIT-YEAR=1604:1604-01-01`
-- https://github.com/nextcloud/server/issues/3084
birthdayParser :: Maybe Year -> Parser Birthday
birthdayParser maybeExpectedYear = do
  maybeYear <- case maybeExpectedYear of
    Just expectedYear -> do
      let expectedYearString = show expectedYear
      string (T.pack expectedYearString) <?> ("year " <> expectedYearString)
      pure Nothing

    Nothing -> do
      year <- read <$> count 4 digitChar <?> "year"
      pure . Just $ year

  optional $ char '-'
  month <- read <$> count 2 digitChar <?> "month"
  optional $ char '-'
  day <- read <$> count 2 digitChar <?> "day"

  pure $ case maybeYear of
    Just year -> Full $ fromGregorian year month day
    Nothing -> Partial month day

newtype Contact = Contact (Name, Birthday)
  deriving Show

vcardParser :: Parser (Maybe Contact)
vcardParser = runMaybeT $ do
  -- Parser () => Parser (Maybe ()) => MaybeT Parser ()
  MaybeT $ fmap Just $ string "BEGIN:VCARD" *> eol
  keyValues <- MaybeT $ fmap Just $ someTill contentline (string "END:VCARD" *> eol)
  let (Just fName) = snd <$> find ((== "FN") . fst) keyValues
  (bDay :: T.Text) <- MaybeT $ pure $ snd <$> find ((== "BDAY") . fst) keyValues
  let bDayResult = parse (birthdayParser Nothing) "" bDay
  case bDayResult of
    -- TODO adjust the error position according to the original parser
    Left errorBundle -> MaybeT . parseError . NE.head . bundleErrors $ errorBundle
    Right bDay -> pure $ Contact (Name fName, bDay)

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
vcardsParser = do
  vcards <- some vcardParser <* eof
  pure $ catMaybes vcards

someFunc :: IO ()
someFunc = putStrLn "someFunc"
