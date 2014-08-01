module Main where

import           Control.Applicative
import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever)
import qualified Data.Text           as T
import           System.Environment  (getArgs)

import           Bot
import           Markov
import           Tweet

second, minute, hour :: Int
second = 1000000
minute = 60 * second
hour   = 60 * minute

main :: IO ()
main = do
  argPairs <- zip <*> tail <$> getArgs
  case argPairs of
    [] -> error "Usage: RapCandy -c <path/to/config.json>"
    _  -> return ()
  let conf = lookup "-c" argPairs
  case conf of
    Nothing -> error "Please pass filepath to config.json as argument to -c"
    Just path -> do
      cfg <- configFromFile path
      case cfg of
        Left _ -> error "Trouble parsing configuration file."
        Right config -> forever $ do
          status <- randomTweet
          tweet config (T.unpack status)
          threadDelay (30 * minute)
