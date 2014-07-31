module Main where

import Data.Text as T
import Control.Concurrent (threadDelay)
import Control.Monad(forever)

import Markov
import Bot

second, minute, hour :: Int
second = 1000000
minute = 60 * second
hour   = 60 * minute

main :: IO ()
main = forever $ do
  status <- randomTweet
  tweet (T.unpack status)
  threadDelay (30 * minute)