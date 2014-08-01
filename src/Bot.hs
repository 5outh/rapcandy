{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Bot where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (fromJust)
import           GHC.Generics
import qualified Network.HTTP.Base       as HTTP
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Web.Authenticate.OAuth

data Config = Config {
    oauthKey    :: String,
    oauthSecret :: String,
    appKey      :: String,
    appSecret   :: String
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

oauthTwitter :: ByteString -> ByteString -> OAuth
oauthTwitter key secret =
  newOAuth { oauthServerName      = "twitter"
           , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
           , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
           , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
           , oauthSignatureMethod = HMACSHA1
           , oauthConsumerKey     = key
           , oauthConsumerSecret  = secret
           , oauthVersion         = OAuth10a
           }

signWithConfig :: Config -> Request -> IO Request
signWithConfig Config{..} = signOAuth
  (oauthTwitter (B.pack oauthKey) (B.pack oauthSecret))
  (newCredential (B.pack appKey) (B.pack appSecret))

configFromFile :: FilePath -> IO (Either String Config)
configFromFile path = do
  contents <- BL.readFile path
  return $ eitherDecode contents

tweet :: Config -> String -> IO (Response BL.ByteString)
tweet config status = do
  url <- parseUrl $ "https://api.twitter.com/1.1/statuses/update.json?status=" ++ HTTP.urlEncode status
  req <- signWithConfig config url{ method = "POST" }
  manager <- newManager tlsManagerSettings
  httpLbs req manager
