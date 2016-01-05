{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module BD.Types where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import GHCJS.Types (JSString)
import Data.Monoid
import Data.JSString.Text (textFromJSString, textToJSString)

type Text = JSString

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON