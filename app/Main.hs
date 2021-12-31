module Main where

import qualified Data.Text.IO as T (getContents)

import Lib (printB2C)

main :: IO ()
main = T.getContents >>= printB2C
