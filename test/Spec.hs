{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Header
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.UTF8 as U
import Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /api/phonenumbers/parse/text" $ do
        it "responds with 200" $ do
            get "/api/phonenumbers/parse/text" `shouldRespondWith` 200
        it "responds with empty json list" $ do
            get "/api/phonenumbers/parse/text" `shouldRespondWith`  [json|[]|]
        it "responds with a json list of parsed number" $ do
            let phonenumber = "+14165555555"
                url     = "/api/phonenumbers/parse/text/" <> phonenumber
                jsonPhoneResponse = [json|["+1 416-555-5555"]|]
            get url `shouldRespondWith` jsonPhoneResponse
        it "responds with status 400 with wrong format number" $ do
            let phonenumber = "14165555555"
                url     = "/api/phonenumbers/parse/text/" <> phonenumber
            get url `shouldRespondWith` 400
        it "responds with status 200 with base64 encode phones" $ do
            let phonenumber       = "+14165555555, +14165555555" :: B.ByteString
                phonenumberBase64 = C.pack . U.toString . B64.encode $ phonenumber
                url     = "/api/phonenumbers/parse/text/"
                header = [("Content-Type", "text/plain;charset=UTF-8"), ("Accept", "application/json")]
                jsonPhoneResponse = [json|["+1 416-555-5555", "+1 416-555-5555"]|]
            (request methodPost url header phonenumberBase64) `shouldRespondWith` jsonPhoneResponse { matchStatus = 200 }
