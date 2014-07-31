{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Bot where

import Data.Maybe (fromJust)
import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Monad.IO.Class
import Data.ByteString
import Control.Monad

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
auth = oauthTwitter "I2u0NYuLXMP1WKjS3384qe0vG" 
                    "cRvdQMS3WI2POJfN8Y4T4cqMe2vrg32kCn2l8DXijQz7ihFcrF"

credential :: Credential
credential = newCredential "436362160-8mEQeSa3lfZV6L6KNqIgtO1X8R9Aq8JjDUaW8Mem" 
                           "thWcMDsYz1gxV1laSO95RGmRa4NZMx9pHuYNG2bfuKZ30"

--withOAuth :: MonadIO m => Request -> m Request
withOAuth = signOAuth auth credential

req = do
  url <- parseUrl "https://api.twitter.com/1.1/statuses/update.json?status=TESTING"
  withOAuth url{ method = "POST" }  

testGo = do
  manager <- newManager tlsManagerSettings
  request <- req
  res     <- httpLbs request manager
  print $ responseBody res

