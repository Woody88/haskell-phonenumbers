{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts   #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Server
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as C
import PhoneNumbers       as P
import PhoneNumbers.Types as P

type PhoneNumberFile = B.ByteString

type API =  "api" :> "phonenumbers" :> "parse" :> "text" :> Get '[JSON] PhoneNumberFormats
       :<|> "api" :> "phonenumbers" :> "parse" :> "text" :> Capture "phone" Text :> Get '[JSON] PhoneNumberFormats
       :<|> "api" :> "phonenumbers" :> "parse" :> "text" :> ReqBody '[PlainText] Text :> Post '[JSON] PhoneNumberFormats

startApp :: IO ()
startApp = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        serverRunningMessage >> runSettings settings app
    where port = 8080
          serverRunningMessage = putStrLn $ "Server Running on port: " ++ show port

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  return noPhone
     :<|> parsePhone
     :<|> parsePhoneFile
     where parsePhone p        = parsePhone' p
           parsePhoneFile file = parsePhoneFile' file

parsePhone' :: T.Text -> Handler PhoneNumberFormats
parsePhone' p =  do
    result <- liftIO $ P.parseInternational p
    appHandler $ result
    where pHandler (Right result)    = return result
          pHandler (Left errMessage) = throwError $ err400 { errBody = errMessage }


parsePhoneFile' :: T.Text -> Handler PhoneNumberFormats
parsePhoneFile' base64Text = parseB64 $ B64.decode $ T.encodeUtf8 base64Text
    where parseB64 (Left t)  = throwError $ err400 { errBody = "Could not parse base64 text" }
          parseB64 (Right t) = do { result <- liftIO $ P.parseMatcher . T.decodeUtf8 $ t; appHandler $ result }


noPhone :: PhoneNumberFormats
noPhone = []

appHandler :: Either C.ByteString b -> Handler b
appHandler (Right result)    = return result
appHandler (Left errMessage) = throwError $ err400 { errBody = errMessage }
