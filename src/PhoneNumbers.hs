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
import PhoneNumbers.Class (initializePhoneNumber, parsePhoneMatcher, parse, formatNumber)
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import System.IO (stdout)
import qualified CPython as Py
import qualified CPython.Types.Unicode as Py hiding (length)
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Exception as Py


initialize :: IO ()
initialize = Py.initialize

parseInternational :: T.Text -> IO PhoneNumberFormats
parseInternational = \p -> do
 Py.initialize
 pyPhoneModule    <- initializePhoneNumber
 phone           <- parse pyPhoneModule p None
 (\pf -> [pf] ) <$> formatNumber pyPhoneModule (phone, INTERNATIONAL)

parseMatcher :: T.Text -> IO PhoneNumberFormats
parseMatcher = \text -> do
 Py.initialize
 pyPhoneModule    <- initializePhoneNumber
 parsePhoneMatcher pyPhoneModule text

--
--   -- phone <- parse phonenumbers "+1613321114" None
--   -- (PhoneNumberFormat phoneNational) <- formatNumber phonenumbers (phone, INTERNATIONAL)
--   -- i <- Py.iterableToList v
--   -- l <- Py.length i
--   print $ length v
--   return ()
