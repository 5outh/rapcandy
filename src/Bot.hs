{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Bot where

import Data.Maybe (fromJust)
import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.IO.Class
import Data.ByteString

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
auth = oauthTwitter "k1Gd4nLdXLXgHx1HwQ2S05F3J" "gi75TE8dJHqZJS1l8yBWkYI7wIx2fl616c4nEFXASaaFrA8yPL"

credential :: Credential
credential = newCredential "436362160-dCjYnNn7lEVHVNAnMSaTPzWMMbMwpqQ9S25k2ZZC" "jtoqYgGcNG3MMdo6lEqSgXarbqeCFEtJRjMGBN3gSc1GX"

--withOAuth :: MonadIO m => Request -> m Request
withOAuth = signOAuth auth credential

-- req :: (MonadIO m, MonadThrow m) => m Request
req = do
  url <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=Bendotk"
  withOAuth url

mgr = newManager defaultManagerSettings

testGo = do
  manager <- newManager tlsManagerSettings
  request <- req
  res     <- httpLbs request manager
  print res

--{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--import Data.ByteString (ByteString)
--import Network.HTTP.Client
--import Web.Authenticate.OAuth
--import Data.Aeson
--import Data.Time.Clock (UTCTime)
--import Data.Text (Text)
--import GHC.Generics

---- Insert here your own credentials

--myoauth :: OAuth
--myoauth =
--  newOAuth { oauthServerName     = "api.twitter.com"
--           , oauthConsumerKey    = "k1Gd4nLdXLXgHx1HwQ2S05F3J"
--           , oauthConsumerSecret = "gi75TE8dJHqZJS1l8yBWkYI7wIx2fl616c4nEFXASaaFrA8yPL"
--           }

--mycred :: Credential
--mycred = newCredential "436362160-dCjYnNn7lEVHVNAnMSaTPzWMMbMwpqQ9S25k2ZZC" "jtoqYgGcNG3MMdo6lEqSgXarbqeCFEtJRjMGBN3gSc1GX"

---- | Type for tweets. Use only the fields you are interested in.
----   The parser will filter them. To see a list of available fields
----   see <https://dev.twitter.com/docs/platform-objects/tweets>.
--data Tweet =
--  Tweet { text :: !Text
--        , created_at :: !UTCTime
--          } deriving (Show, Generic)

--instance FromJSON Tweet
--instance ToJSON Tweet

---- | This function reads a timeline JSON and parse it using the 'Tweet' type.
--timeline :: String -- ^ Screen name of the user
--         -> IO (Either String [Tweet]) -- ^ If there is any error parsing the JSON data, it
--                                       --   will return 'Left String', where the 'String'
--                                       --   contains the error information.
--timeline name = do
--  -- Firstly, we create a HTTP request with method GET (it is the default so we don't have to change that).
--  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
--  -- Using a HTTP manager, we authenticate the request and send it to get a response.
--  manager <- newManager defaultManagerSettings
--  res <- withManager defaultManagerSettings $ \m -> do
--           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
--           -- appropriate authentication.
--           signedreq <- signOAuth myoauth mycred req
--           -- Send request.
--           httpLbs signedreq m
--  -- Decode the response body.
--  return $ eitherDecode $ responseBody res

---- | The main function, as an example of how to use the 'timeline'
----   function.
--main :: IO ()
--main = do
--  -- Read the timeline from Hackage user. Feel free to change the screen
--  -- name to any other.
--  ets <- timeline "Hackage"
--  case ets of
--   -- When the parsing of the JSON data fails, we report it.
--   Left err -> putStrLn err
--   -- When successful, print in the screen the first 5 tweets.
--   Right ts  -> mapM_ print $ take 5 ts

