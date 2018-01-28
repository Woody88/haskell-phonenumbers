{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
module PhoneNumbers.Types where

import Data.Aeson
import Data.Aeson.TH
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Module as Py
import           Data.Text                  (Text)
import qualified Data.Text as T

data PhoneNumberModule = PhoneNumberModule (Py.Module)
data PhoneNumber       = PhoneNumber (Py.SomeObject)
data PhoneNumberFormat = PhoneNumberFormat (Text) deriving (Show, Eq)
data PhoneNumberMatcher = PhoneNumberMatcher [Py.SomeObject]
data NoPhone = NoPhone  deriving Show

$(deriveJSON defaultOptions ''PhoneNumberFormat)

data Region
 = NATIONAL
 | INTERNATIONAL
 | E164
 deriving Show

data CountryCode
 = CA
 | GB
 | None
 deriving Show

type PhoneNumberRegion  = (PhoneNumber, Region)
type PhoneNumberFormats = [PhoneNumberFormat]
