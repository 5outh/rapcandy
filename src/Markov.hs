{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, RecordWildCards #-}
module Markov where

import qualified Data.Map as M
import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Data.List
import Control.Arrow
import Control.Applicative
import Data.Monoid
import Data.Ord
import Data.Function
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import Data.String

newtype Markov g a = Markov{ getMarkov :: M.Map a (Maybe (Rand g a)) }

type MarkovI a = M.Map a (Maybe [(a, Rational)])

data Outcome g a = 
    Error String
  | Val a g 
  | End
    deriving (Show, Eq)

runMarkov1 :: (RandomGen g, Ord a, Show a, IsString a) => Markov g a -> g -> a -> Outcome g a
runMarkov1 mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Val "" $ snd (next gen)
  Just rs -> case flip runRand gen <$> rs of
    Nothing -> End
    Just (a, g) -> Val a g

runMarkov :: (RandomGen g, Ord a, Show a, Num n, Ord n, IsString a) => n -> Markov g a -> g -> a -> Either String [a]
runMarkov n mkv gen x = go n
  where 
    go m | m <= 0 = Right []
         | otherwise = (x:) <$> case runMarkov1 mkv gen x of
            Val a g -> runMarkov (n-1) mkv g a
            End -> Right []
            Error err -> Left err 

rle :: Ord a => [a] -> [(a, Rational)]
rle = map (head &&& (fromIntegral . length)) . group

markovi :: Ord a => [a] -> MarkovI a
markovi xs =   M.map (wrapMaybe . rle) 
            $ foldr (uncurry accum) wordMap withNexts
  where withNexts = zip xs (tail xs)
        wordMap = foldr (`M.insert` []) M.empty xs
        accum w nxt = M.adjust (nxt:) w
        wrapMaybe x = case x of { [] -> Nothing; xs -> Just xs }

markov :: (RandomGen g, Ord a) => [a] -> Markov g a
markov = Markov . M.map (fmap fromList) . markovi 

fromMarkovI :: RandomGen g => MarkovI a -> Markov g a
fromMarkovI = Markov . M.map (fromList <$>)

getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles file = filter (`notElem` [".", ".."]) <$> getDirectoryContents file

clean :: T.Text -> T.Text
clean = T.filter (`notElem` "[]:(){}\"<>\\/")

notGarbage :: T.Text -> Bool
notGarbage "" = False
notGarbage t =
     T.head t `notElem` "-[]':(){}\"*!#"
  && T.last t `notElem` "[]':(){}\"*!#"
  && not (T.isPrefixOf "Chorus" t)

insertMkvI :: Ord a => a -> a -> MarkovI a -> MarkovI a
insertMkvI k v mkv = M.insert k (Just $ case M.lookup k mkv of
  Nothing -> [(v, 1)]
  Just xs -> case xs of
    Nothing -> [(v, 1)]
    Just ys -> (v, 1):ys) mkv

insertEnd :: Ord a => a -> MarkovI a -> MarkovI a
insertEnd k = M.insert k Nothing

insertMkvPairsInto :: Ord a => MarkovI a -> [(a, a)] -> MarkovI a
insertMkvPairsInto mkv [] = mkv
insertMkvPairsInto mkv ps = insertEnd lst $ foldl' (flip (uncurry insertMkvI)) mkv ps
  where lst = snd $ last ps

insertTextInto :: MarkovI T.Text -> T.Text -> MarkovI T.Text
insertTextInto mkv t = insertMkvPairsInto mkv (zip wds (tail wds))
  where wds = map clean $ filter notGarbage $ T.words t

fromTexts :: [T.Text] -> MarkovI T.Text
fromTexts = foldl' insertTextInto M.empty

cleanText :: T.Text -> T.Text
cleanText = T.unwords . map clean . filter notGarbage . T.words

songs :: IO (Markov PureMT T.Text, [T.Text])
songs = do 
  files <- fmap ("./scraper/lyrics/" <>) <$> getDirectoryFiles "./scraper/lyrics"
  lns   <- concatMap (map cleanText . T.lines) <$> mapM TIO.readFile files
  let hds = filter ((`elem` ['A'..'Z']) . T.head) $ mapMaybe (shead . T.words) lns
  g    <- newPureMT
  seed <- uniform hds
  return (fromMarkovI (fromTexts lns), hds)

shead :: [a] -> Maybe a
shead []    = Nothing
shead (x:_) = Just x

data GoState = GoState
  { mkv :: Markov PureMT T.Text
  , hds :: [T.Text]
  , lst :: T.Text
  , acc :: T.Text
  , start :: Bool
  }

go :: GoState -> IO T.Text
go gs@GoState{..}
  | T.length acc > 140 && not (T.null lst) = return $ if T.last lst == ',' then T.init lst else lst
  | otherwise = do
    seed <- uniform hds
    g    <- newPureMT
    let res = T.unwords <$> runMarkov 25 mkv g seed
    case res of
      Left err  -> error err
      Right str -> go $ if T.length str < 20 then gs
                         else gs { lst = acc, acc = acc <> if start then "" else "\n" <> str, start = False }

randomTweet' :: Markov PureMT T.Text -> [T.Text] -> IO T.Text
randomTweet' mkv seeds = 
  go $ GoState mkv seeds "" "" True

randomTweet :: IO T.Text
randomTweet = songs >>= uncurry randomTweet'
