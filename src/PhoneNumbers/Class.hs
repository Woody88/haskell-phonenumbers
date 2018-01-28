{-# LANGUAGE OverloadedStrings #-}
module PhoneNumbers.Class where

import Control.Monad.IO.Class
import Control.Monad (foldM)
import Data.Monoid ((<>))
import PhoneNumbers.Types
import System.IO (stdout)
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified CPython as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Unicode as Py hiding (length)
import qualified CPython.Types.List as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Exception as Py
import qualified CPython.Constants as Py


class PhoneNumberParser a where
 parse :: a -> T.Text -> CountryCode -> IO (PhoneNumber)
 formatNumber :: a -> PhoneNumberRegion -> IO (PhoneNumberFormat)
 toFunc :: a -> T.Text -> IO Py.SomeObject
 parsePhoneMatcher :: a -> T.Text -> IO PhoneNumberFormats


phonenumberPythonModule :: T.Text
phonenumberPythonModule = "phonenumbers"

builtinsPythonModule :: T.Text
builtinsPythonModule = "builtins"

toText :: Py.SomeObject -> IO T.Text
toText = \o -> Py.fromUnicode =<< Py.string o


toFuncText :: T.Text -> IO Py.SomeObject
toFuncText = \t -> Py.toObject <$> Py.toUnicode t

initializeBuiltinModule :: IO Py.Module
initializeBuiltinModule = Py.importModule builtinsPythonModule

initializePhoneNumber :: IO PhoneNumberModule
initializePhoneNumber = initializeModule <$> (Py.importModule phonenumberPythonModule)
 where initializeModule :: Py.Module -> PhoneNumberModule
       initializeModule = \p -> PhoneNumberModule p

onException :: Py.Exception -> IO ()
onException exc = Py.print (Py.exceptionValue exc) stdout

instance PhoneNumberParser PhoneNumberModule where
 toFunc = \(PhoneNumberModule m) t -> Py.getAttribute m =<< Py.toUnicode t
 parse = \s n c -> do
  parseFunc <- toFunc s "parse"
  country   <- parseCountryCode c
  number    <- toFuncText n
  parse' parseFunc number country

  where parseCountryCode :: CountryCode -> IO Py.SomeObject
        parseCountryCode None = Py.none
        parseCountryCode ctry = toFuncText $ T.pack $ show ctry
        parse' :: Py.SomeObject -> Py.SomeObject -> Py.SomeObject -> IO PhoneNumber
        parse' f num ctry = Py.callArgs f [num, ctry] >>= (\v -> return $ PhoneNumber v)

 formatNumber = \s (n,r) -> do
   formatMod    <- toFunc s "PhoneNumberFormat"
   formatFunc   <- toFunc s "format_number"
   region       <- pyRegion formatMod $ T.pack $ show r
   formatNumber' formatFunc n region

  where pyRegion formatModule reg = Py.getAttribute formatModule =<< Py.toUnicode reg
        formatNumber'             = \f (PhoneNumber n') r' -> Py.callArgs f [n', r'] >>= (\v -> toText v >>= (\t -> return $ PhoneNumberFormat t))

 parsePhoneMatcher = \s txt -> do
  builtins         <- initializeBuiltinModule
  listFunc         <- Py.getAttribute builtins =<< Py.toUnicode "list"
  phoneMatcherFunc <- toFunc s "PhoneNumberMatcher"
  country          <- Py.toObject <$> Py.toUnicode "CA"
  t                <- Py.toObject <$> Py.toUnicode txt
  matchers         <- Py.callArgs phoneMatcherFunc [t, country] >>= Py.iterableToList >>= Py.fromList
  formatMatchers matchers s

  where formatMatchers m s' = tail <$> foldM (\x b-> do
                                               num   <- Py.getAttribute b =<< Py.toUnicode "number"
                                               value <- formatNumber s' ((PhoneNumber num), INTERNATIONAL)
                                               return $ x ++ [value]
                                               ) [(PhoneNumberFormat "")] m


-- main :: IO ()
-- main = do
--  Py.initialize
--  E.handle onException $ do
--   -- parseInternational' "+1613321114"
--   let text = ["+15107488230 if it's before 9:30, or on +17034800500 after 10am.", "yo"]
--   phMod@(PhoneNumberModule phonenumbers)   <- initializePhoneNumber
--   builtins <-Py.importModule "builtins"
--   toList <-Py.getAttribute builtins =<< Py.toUnicode "list"
--   phonematcher <- Py.getAttribute phonenumbers =<< Py.toUnicode "PhoneNumberMatcher"
--   country <- Py.toObject <$> Py.toUnicode "CA"
--   t <- Py.toObject <$> Py.toUnicode text
--   v <- Py.callArgs phonematcher [t, country] >>= Py.iterableToList >>= Py.fromList
--   p <- foldM (\x b-> do
--                  num <- Py.getAttribute b =<< Py.toUnicode "number"
--                  value <- formatNumber phMod ((PhoneNumber num), INTERNATIONAL)
--                  return $ x ++ [value]
--                  ) [(PhoneNumberFormat "")] v
--   print p
--   return ()
