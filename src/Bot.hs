{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, DeriveGeneric, RecordWildCards #-}
module Bot where

import Data.Maybe (fromJust)
import Web.Authenticate.OAuth
import qualified Network.HTTP.Base as HTTP
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Data.ByteString
import qualified Data.ByteString.Char8 as B
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import GHC.Generics


data Config = Config {
    oauthKey :: String,
    oauthSecret :: String,
    appKey :: String,
    appSecret :: String
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

auth :: OAuth
auth = oauthTwitter "TwFoEwY1i6lTN6bPCz03gOG5y" 
                    "S1JTcHMAniaABvJywMdF4z5se1MMkcjgNmKH905eBAtgDOsFpR"

credential :: Credential
credential = newCredential "2696415289-pRPx0TyuFZW7BjjHTDiX2g13qn8vl9N0QZJSzVP" 
                           "B1FnzuF4Yodb2g7qr8bDmuJMieXHgCaxXLhX4BgM5gyjj"

withOAuth :: Request -> IO Request
withOAuth = signOAuth auth credential

signWithConfig :: Config -> Request -> IO Request
signWithConfig Config{..} = signOAuth 
  (oauthTwitter (B.pack oauthKey) (B.pack oauthSecret)) 
  (newCredential (B.pack appKey) (B.pack appSecret))

configFromFile :: FilePath -> IO (Either String Config)
configFromFile path = do
  contents <- BL.readFile path
  return $ eitherDecode contents 

mkStatusReq :: String -> IO Request
mkStatusReq status = do
  url <- parseUrl $ "https://api.twitter.com/1.1/statuses/update.json?status=" ++ HTTP.urlEncode status
  withOAuth url{ method = "POST" }

tweetWithConfig :: Config -> String -> IO (Response BL.ByteString)
tweetWithConfig config status = do
  url <- parseUrl $ "https://api.twitter.com/1.1/statuses/update.json?status=" ++ HTTP.urlEncode status
  req <- signWithConfig config url{ method = "POST" }
  manager <- newManager tlsManagerSettings
  httpLbs req manager

tweet :: String -> IO (Response BL.ByteString)
tweet status = do
  manager <- newManager tlsManagerSettings
  request <- mkStatusReq status
  httpLbs request manager