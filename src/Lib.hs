{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Data.Char (ord)
import Data.Functor (void)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void
import Text.Read (readMaybe)

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

{-
 - contentline  = [group "."] name *(";" param ) ":" value CRLF
 - param        = param-name "=" param-value *("," param-value)
 -
 - [ "FN": (Just "Name", [])
 - , "BDAY": (Just "19900101", [ "X-APPLE-OMIT-YEAR": ("1900", "1900-05-05") ])
 - ]
 -}

--newtype ParamName = ParamName Text
  --deriving Show

--newtype Values = Values (Maybe Text, [ParamName

type VCName = Text
type VCParamName = Text
type VCParamValue = Text
data VCValue = VCValue
  { valText :: Text
  , valPos :: SourcePos
  , valOffset :: Int
  }
 deriving Show

data VCContentLine = VCContentLine
  { clName :: VCName
  , clParam :: Maybe (VCParamName, VCParamValue)
  , clValue :: VCValue
  }
  deriving Show

vcardParser :: Parser (Maybe Contact)
vcardParser = runMaybeT $ do
  -- Parser () => Parser (Maybe ()) => MaybeT Parser ()
  MaybeT $ fmap Just $ string "BEGIN:VCARD" *> eol
  keyValues <- MaybeT $ fmap Just $ someTill contentline (string "END:VCARD" *> eol)

  let (Just fName) = valText . clValue <$> find ((== "FN") . clName) keyValues
  (bDayLine :: VCContentLine) <- MaybeT $ pure $ find ((== "BDAY") . clName) keyValues
  let { maybeBDayOmittedYear = do
    param <- clParam bDayLine
    guard $ fst param == "X-APPLE-OMIT-YEAR"
    readMaybe @Integer . T.unpack . snd $ param
  }

  curPosState :: PosState Text <- fmap statePosState . MaybeT . fmap Just $ getParserState
  let
    bDayCLValue = clValue bDayLine
    bDayParserState = State
      { stateInput = valText bDayCLValue
      , stateOffset = valOffset bDayCLValue
      , statePosState = curPosState
        { pstateInput = valText bDayCLValue
        , pstateOffset = valOffset bDayCLValue
        , pstateSourcePos = valPos bDayCLValue
        }
      , stateParseErrors = mempty
      }
    -- nested parser with correct error locations
    -- https://old.reddit.com/r/haskell/comments/q7ytoj/is_there_a_good_way_to_run_an_inner_parser_with/hgnkc8r/
    bDayResult = snd $ runParser' (birthdayParser maybeBDayOmittedYear <* eof) bDayParserState
  case bDayResult of
    Left errorBundle -> MaybeT . parseError . NE.head . bundleErrors $ errorBundle
    Right bDay -> pure $ Contact (Name fName, bDay)

  where
    contentline :: Parser VCContentLine
    contentline = do
      optional . try $ group *> char '.'
      clName <- name
      maybeParam <- fmap safeHead . many $ do
        char ';'
        paramName <- name
        char '='
        paramValue <- many safeChar
        pure (T.pack paramName, T.pack paramValue)
      char ':'
      pos <- getSourcePos
      offset <- getOffset
      clValue <- value
      -- unfolding: https://datatracker.ietf.org/doc/html/rfc2425#section-5.8.1
      clValues <- many $ try (eol *> char ' ' *> value)
      eol

      pure $ VCContentLine (T.pack clName) maybeParam (VCValue (T.pack . concat $ (clValue:clValues)) pos offset)
      -- <?> "contentline"

    name = some $ alphaNumChar <|> char '-' -- satisfy (\c -> c /= ':' && c /= ';') -- oneOf ['-', '.', ';', '=']
    value = many printChar
    group = name
    --   SAFE-CHAR    = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E / NON-ASCII
    --           ; Any character except CTLs, DQUOTE, ";", ":", ","
    safeChar = satisfy (flip S.member safeCharSet . ord)

safeCharSet :: Set Int
safeCharSet = S.fromDistinctAscList $ concat [[ord ' ', 0x21], [0x23..0x2b], [0x2d..0x39], [0x3c..0x7e]]

safeHead :: [a] -> Maybe a
safeHead = fmap fst . uncons

vcardsParser :: Parser [Contact]
vcardsParser = do
  vcards <- some vcardParser <* eof
  pure $ catMaybes vcards

someFunc :: IO ()
someFunc = putStrLn "someFunc"
