{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype Name = Name Text
  deriving Show

type KeyValue = (Text, Text)

vcardParser :: Parser [KeyValue]
vcardParser = do
  string "BEGIN:VCARD" *> eol
  someTill contentline (string "END:VCARD" *> eol)

  where
    --fn = (string "FN:" *> some printChar) <?> "formatted name"
    contentline :: Parser KeyValue
    contentline = do
      name <- name
      char ':'
      value <- value
      eol
      pure (T.pack name, T.pack value)
      -- <?> "contentline"
    name = some $ alphaNumChar <|> anySingleBut ':' -- oneOf ['-', '.', ';', '=']
    value = many printChar

vcardsParser :: Parser Int
vcardsParser = length <$> some vcardParser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
