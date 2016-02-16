{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module BD.Types where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import Data.Monoid

import GHCJS.Types (JSString)
import Data.JSString.Text (textFromJSString, textToJSString)

type Text = JSString

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

data AppError = ApiError JSString | BLError JSString | NotImplementedError JSString
  -- deriving (Show)

data Notification = NError AppError | NInfo JSString | NWarning JSString | NSuccess JSString

apiError     = NError . ApiError
blError      = NError . BLError
notImplError = NError . NotImplementedError

data FormValid e = FormValid | FormNotValid e
type Validator a e = a -> FormValid e

data FormValidIO a = FormValidIO | FormNotValidIO a
type ValidatorIO a e = a -> IO (FormValidIO e)

-- FIXME should be in Ad Platform types probably
data Nav = NavLogin | NavUser | NavCampaign | NavSearch | NavCreateAd | NavImages
  deriving (Show, Eq)
