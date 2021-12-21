module Lib
  ( b2c
  , printB2C
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (stderr)

import Text.Megaparsec

import Calendar (calendarData)
import VCard (vcardsParser)

-- |Parses some `Contact`s from the `input` and returns either a calendar
-- representation of the birthdays or a text with parse errors.
b2c :: T.Text -> IO (Either T.Text BSL.ByteString)
b2c input = do
  let contacts = runParser vcardsParser "" input
  case contacts of
    Right contacts -> do
      output <- calendarData contacts
      pure $ Right output

    Left errors -> pure . Left . T.pack $ errorBundlePretty errors

-- |Wrapper around `b2c` that prints the calendar representation to `stdout`
-- or the errors to `stderr`.
printB2C :: T.Text -> IO ()
printB2C input = either (T.hPutStr stderr) BSL.putStr =<< b2c input
