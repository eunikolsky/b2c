{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either (isLeft, isRight)
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath

import Test.Hspec

import Lib (b2c)

main :: IO ()
main = hspec $ do
  describe "b2c" $ do
    it "returns Right for a valid vCard" $
      b2c "BEGIN:VCARD\r\nVERSION:3.0\r\nN:Name\r\nFN:Full Name\r\nBDAY:2000-12-31\r\nEND:VCARD\r\n" >>= (`shouldSatisfy` isRight)

    it "returns Right for a valid vCard without birthday" $
      b2c "BEGIN:VCARD\r\nVERSION:3.0\r\nN:Name\r\nFN:Full Name\r\nEND:VCARD\r\n" >>= (`shouldSatisfy` isRight)

    it "returns Right for my sample vcf" $ do
      -- from https://stackoverflow.com/questions/30450329/access-test-resources-within-haskell-tests/30467385#30467385
      let contactsFile = takeDirectory __FILE__ </> "contacts.vcf"
      exists <- doesFileExist contactsFile
      if exists
        then do
          vcf <- T.readFile contactsFile
          b2c vcf >>= (`shouldSatisfy` isRight)
        else putStrLn $ "Test file " <> contactsFile <> " not found; skipping the test"

    it "returns Left for an invalid vCard" $
      b2c "BEGIN:VCARD\r\nVERSION:3.0\r\nFN:Full Name\r\nBDAY:2000-12-31\r\nEND:VCARD\r\n" >>= (`shouldSatisfy` isLeft)
