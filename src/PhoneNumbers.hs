{-# LANGUAGE OverloadedStrings #-}

{-| This module will holds all the functions pertinent to communicating
    with the google libphonenumber (Python version) using the cypthon library built by
    John Millikin.

    It is imperative that before the executation of any python call that Py.initialize is executated.
    Py.initialze initializes the Python interpreter. For this case Py.initialize will only be called when
    the server runs.
-}

module PhoneNumbers (parseInternational, parseMatcher, initialize) where

import PhoneNumbers.Types
import PhoneNumbers.Class (initializePhoneNumber, parsePhoneMatcher, parse, formatNumber, toText)
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import System.IO (stdout)
import qualified CPython as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Exception as Py
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (nub)


initialize :: IO ()
initialize = Py.initialize

-- PhoneNumberFormats
parseInternational :: T.Text -> IO (Either C.ByteString PhoneNumberFormats)
parseInternational = \p -> do
 Py.initialize
 E.handle onException $ do
  pyPhoneModule    <- initializePhoneNumber
  phone            <- parse pyPhoneModule p None
  result           <- nub . toList <$> formatNumber pyPhoneModule (phone, INTERNATIONAL)
  return $ Right result
  where toList = \pf -> [pf]

parseMatcher :: T.Text -> IO (Either C.ByteString PhoneNumberFormats)
parseMatcher = \text -> do
 Py.initialize
 pyPhoneModule    <- initializePhoneNumber
 result           <- nub <$> parsePhoneMatcher pyPhoneModule text
 return $ Right result

onException :: Py.Exception -> IO (Either C.ByteString b)
onException exc = do
 errText <- T.unpack <$> toText (Py.exceptionValue exc)
 packToBytestring errText
 where packToBytestring = \t ->  return $ Left $  C.pack t
