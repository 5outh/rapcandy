{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Tweet where

import           Control.Applicative
import           Control.Monad.Random
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Directory              (getDirectoryContents)
import           System.Random.Mersenne.Pure64

import           Markov

shead :: [a] -> Maybe a
shead []    = Nothing
shead (x:_) = Just x

getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles file = filter (`notElem` [".", ".."]) <$> getDirectoryContents file

songs :: IO (Markov PureMT T.Text, [T.Text])
songs = do
  files <- fmap ("./scraper/lyrics/" <>) <$> getDirectoryFiles "./scraper/lyrics"
  lns   <- concatMap T.lines <$> mapM TIO.readFile files
  let hds = filter ((`elem` ['A'..'Z']) . T.head) $ mapMaybe (shead . T.words) lns
  g    <- newPureMT
  seed <- uniform hds
  return (fromMarkovI (fromTexts lns), hds)

data GoState = GoState
  { mkv   :: Markov PureMT T.Text
  , hds   :: [T.Text]
  , lst   :: T.Text
  , acc   :: T.Text
  , start :: Bool
  }

randomTweet' :: Markov PureMT T.Text -> [T.Text] -> IO T.Text
randomTweet' mkv seeds = go $ GoState mkv seeds "" "" True
  where go gs@GoState{..}
          | T.length acc > 140 && not (T.null lst) =
              return $ T.init $ T.tail $ T.unlines $ map (T.unwords . T.words) $ T.lines $ if T.last lst == ',' then T.init lst else lst
          | otherwise = do
              seed <- uniform hds
              g    <- newPureMT
              let res = T.unwords <$> runMarkov 25 mkv g seed
              case res of
                Left err  -> error err
                Right str -> go $ if T.length str < 20
                                  then gs
                                  else gs { lst = acc, acc = acc <> if start then "" else "\n" <> str, start = False }

randomTweet :: IO T.Text
randomTweet = songs >>= uncurry randomTweet'
