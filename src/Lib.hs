{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Functor (void)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype Name = Name Text
  deriving Show

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
