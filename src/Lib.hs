{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
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
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger
import Servant
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import qualified Text.Blaze.Html.Renderer.Utf8 as H (renderHtml)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
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
parsePhone' p = liftIO $ P.parseInternational p

parsePhoneFile' :: T.Text -> Handler PhoneNumberFormats
parsePhoneFile' base64Text = parseB64 $ B64.decode $ T.encodeUtf8 base64Text
    where parseB64 (Left t)  = return [(PhoneNumberFormat "Could not parse base64 text")]
          parseB64 (Right t) = liftIO $ P.parseMatcher . T.decodeUtf8 $ t

noPhone :: PhoneNumberFormats
noPhone = []

-- Detect an error with this run function. It seems like warp cuts off the ';' in the accept head
-- Whic caused problem  when tryin to implement the POST Request. Welkin from #haskell IRC told me that
-- he found this issue last issue.
-- startApp :: IO ()
-- startApp = run 8080 $ logStdoutDev $ app

-- This code will be necessary if I want to serve HTML
-- instance H.ToMarkup Page where
--   toMarkup _ = H.h1 "Hello World!"
--
-- data HTMLBlaze
-- data Page = Page PhoneNumberFormats
-- instance ToJSON Page where
--   toJSON (Page phone) = toJSON phone
-- instance Accept HTMLBlaze where
--   contentType _ = "text" // "html" /: ("charset", "utf-8")
--
-- instance H.ToMarkup a => MimeRender HTMLBlaze a where
--   mimeRender _ = H.renderHtml . H.toHtml

-- while we're at it, just like for lucid we can
-- provide an instance for rendering blaze's 'Html' type
-- :<|> "api" :> "phonenumbers" :> "parse" :> "text" :> ReqBody '[PlainText] PhoneNumberFile :> POST '[JSON] Page
-- instance MimeRender HTMLBlaze H.Html where
--   mimeRender _ = H.renderHtml
