{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Bot(tweet) where

import Data.Maybe (fromJust)
import Web.Authenticate.OAuth
import qualified Network.HTTP.Base as HTTP
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Monad.IO.Class
import Data.ByteString
import Control.Monad
import qualified Data.ByteString.Lazy.Internal as BL

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

mkStatusReq status = do
  url <- parseUrl $ "https://api.twitter.com/1.1/statuses/update.json?status=" ++ HTTP.urlEncode status
  withOAuth url{ method = "POST" }

tweet :: String -> IO (Response BL.ByteString)
tweet status = do
  manager <- newManager tlsManagerSettings
  request <- mkStatusReq status
  httpLbs request manager