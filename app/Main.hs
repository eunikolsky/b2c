module Main where

import qualified Data.ByteString.Lazy as BSL (putStr)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (getContents, hPutStr)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (stderr)

import Lib (b2c)

main :: IO ()
main = T.getContents >>= printB2C

-- |Wrapper around `b2c` that prints the calendar representation to `stdout`
-- or the errors to `stderr` (and exits with error code 1 in this case).
printB2C :: T.Text -> IO ()
printB2C input = either printErrors BSL.putStr =<< b2c input
  where
    printErrors :: T.Text -> IO ()
    printErrors errors = T.hPutStr stderr errors *> exitWith (ExitFailure 1)
