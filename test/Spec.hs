{-# LANGUAGE OverloadedStrings #-}

import Data.Either (isLeft, isRight)
import Control.Monad.Trans (lift)

import Test.Hspec

import Lib (b2c)

main :: IO ()
main = hspec $ do
  describe "b2c" $ do
    it "returns Right for a valid vCard" $
      b2c "BEGIN:VCARD\r\nVERSION:3.0\r\nN:Name\r\nFN:Full Name\r\nBDAY:2000-12-31\r\nEND:VCARD\r\n" >>= (`shouldSatisfy` isRight)

    it "returns Right for a valid vCard without birthday" $
      b2c "BEGIN:VCARD\r\nVERSION:3.0\r\nN:Name\r\nFN:Full Name\r\nEND:VCARD\r\n" >>= (`shouldSatisfy` isRight)

    it "returns Left for an invalid vCard" $
      b2c "BEGIN:VCARD\r\nVERSION:3.0\r\nFN:Full Name\r\nBDAY:2000-12-31\r\nEND:VCARD\r\n" >>= (`shouldSatisfy` isLeft)
