{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Functor (void)
import Data.Text (Text)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

vcardParser :: Parser ()
vcardParser = do
  string "BEGIN:VCARD" *> eol
  (void (string "END:VCARD") <|> void(some contentline)) *> eol
  pure ()

  where
    contentline = some printChar <?> "contentline"

vcardsParser :: Parser Int
vcardsParser = length <$> some vcardParser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
