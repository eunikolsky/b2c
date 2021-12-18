{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Char (ord)
import Data.Functor (void)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Monoid (Any(..), Last(..), getAny, getLast)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void
import Text.Read (readMaybe)

import Data.Default
import Text.Megaparsec hiding (State)
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

vcardParser :: Parser (Maybe Contact)
vcardParser = runMaybeT $ do
  -- Parser () => Parser (Maybe ()) => MaybeT Parser ()
  lift $ string "BEGIN:VCARD" *> eol
  -- runParserT' :: Monad m => ParsecT e s m a -> State s e -> m (State s e, Either (ParseErrorBundle s e) a)
  -- ( m = ContactState = Control.Monad.State ContactBuilder )
  -- runParserT' @ContactState :: ParsecT e s ContactState a -> State s e -> ContactState (State s e, Either …)

  -- runStateT :: StateT s m a -> s -> m (a, s)
  -- runParser' :: Parsec e s a -> State s e -> (State s e, Either (ParseErrorBundle s e) a)
  -- m = ContactState = Control.Monad.StateT ContactBuilder (Parsec e s)
  -- runStateT (_ :: ContactState ()) :: ContactBuilder -> Parser ((), ContactBuilder)
  -- execStateT (_ :: ContactState ()) :: ContactBuilder -> Parser ContactBuilder
  ContactBuilder
    { cbName = Just name
    , cbBirthday = maybeBirthday :: Maybe Birthday
    , cbVersionCorrect = versionCorrect
    , cbNameComponentsPresent = nameComponentsPresent
    }
    <- lift $ execWriterT (skipSomeTill contentline (string "END:VCARD"))
  lift $ do
    unless versionCorrect $ fail "vCard must have a VERSION"
    unless nameComponentsPresent $ fail "vCard must have the N type"
  -- `eol` parsing is related to "END:VCARD" but happens after verifying
  -- `versionCorrect` because if it's not, the parser should report the error
  -- on that line
  lift eol

  -- _ :: Maybe Birthday -> MaybeT Parser Birthday
  -- MaybeT :: m (Maybe a) -> MaybeT m a
  birthday :: Birthday <- MaybeT $ pure maybeBirthday
  pure $ Contact (name, birthday)

  where
    -- every content line is parsed, but modifies `ContactBuilder` instead of returning anything
    contentline :: ContactParser ()
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
      case clName of
        "FN" -> do
          fn <- value
          -- unfolding: https://datatracker.ietf.org/doc/html/rfc2425#section-5.8.1
          fnParts <- many $ try (eol *> char ' ' *> value)
          tell $ mempty { cbName = Just . Name . T.pack . concat $ (fn:fnParts) }

        "VERSION" -> do
          string "3.0"
          tell $ mempty { cbVersionCorrect = True }

        "N" -> do
          value >> (skipMany $ try (eol *> char ' ' *> value))
          tell $ mempty { cbNameComponentsPresent = True }

        "BDAY" -> do
          let { maybeBDayOmittedYear = do
            param <- maybeParam
            guard $ fst param == "X-APPLE-OMIT-YEAR"
            readMaybe @Integer . T.unpack . snd $ param
          }

          -- TODO support unfolding when parsing birthday value
          bday <- lift $ birthdayParser maybeBDayOmittedYear
          tell $ mempty { cbBirthday = Just bday }

        _ -> do
          value >> (skipMany $ try (eol *> char ' ' *> value))
      void eol

    --unfoldableValue :: (Monad m, Token s ~ Char) => m [Token s]
    --unfoldableValue = many printChar

    name = some $ alphaNumChar <|> char '-' -- satisfy (\c -> c /= ':' && c /= ';') -- oneOf ['-', '.', ';', '=']
    value = many printChar
    group = name
    --   SAFE-CHAR    = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E / NON-ASCII
    --           ; Any character except CTLs, DQUOTE, ";", ":", ","
    safeChar = satisfy (flip S.member safeCharSet . ord)

type ContactParser = WriterT ContactBuilder Parser

data ContactBuilder = ContactBuilder
  { cbName :: Maybe Name
  , cbBirthday :: Maybe Birthday
  , cbVersionCorrect :: Bool -- whether "VERSION:3.0" is present, required for all vCards
  , cbNameComponentsPresent :: Bool -- whether the "N" type is present, required for all vCards
  }
  deriving Show

instance Semigroup ContactBuilder where
  ContactBuilder lName lBirthday lVersionCorrect lNameComponentsPresent
    <> ContactBuilder rName rBirthday rVersionCorrect rNameComponentsPresent
    = ContactBuilder
      { cbName = getLast $ foldMap Last [lName, rName]
      , cbBirthday = getLast $ foldMap Last [lBirthday, rBirthday]
      , cbVersionCorrect = getAny $ foldMap Any [lVersionCorrect, rVersionCorrect]
      , cbNameComponentsPresent = getAny $ foldMap Any [lNameComponentsPresent, rNameComponentsPresent]
      }

instance Monoid ContactBuilder where
  mempty = ContactBuilder
    { cbName = Nothing
    , cbBirthday = Nothing
    , cbVersionCorrect = False
    , cbNameComponentsPresent = False
    }

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
