{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings, GADTs, DeriveGeneric, DeriveDataTypeable #-}

module BD.Api where

import Control.Monad
import Data.Aeson -- TODO proper
import Data.Data
import Data.Text(Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types
import qualified GHC.Generics as GHC
import JavaScript.Web.XMLHttpRequest -- TODO
import GHCJS.Types (JSString)
import Data.Monoid

getAPI :: FromJSON a => JSString -> IO a
getAPI q = do
  r <- xhrByteString r
  case contents r of
    Nothing          -> error "TODO no response"
    Just byteString  -> case Data.Aeson.decodeStrict byteString of
      Nothing -> error "TODO parse error"
      Just x -> return x
  where
    r = Request {
            reqMethod          = GET
          , reqURI             = "http://data.beautifuldestinations.com/api/v1/"
                                    <> q
          , reqLogin           = Nothing
          , reqHeaders         = []
          , reqWithCredentials = False
          , reqData            = NoData
          }

data Envelope a = Envelope { payload :: a } deriving (GHC.Generic,Show, Eq, Data, Typeable)

instance ToJSON a => ToJSON (Envelope a)
instance FromJSON a => FromJSON (Envelope a)
