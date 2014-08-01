{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Markov where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.List                     (foldl')
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import           Data.String                   (IsString)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Random.Mersenne.Pure64

type MarkovI a = M.Map a (Maybe [(a, Rational)])
newtype Markov g a = Markov{ getMarkov :: M.Map a (Maybe (Rand g a)) }
data Outcome g a =
    Error String
  | Val a g
  | End
    deriving (Show, Eq)

runMarkov1 :: (RandomGen g, Ord a) => Markov g a -> g -> a -> Outcome g a
runMarkov1 mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Error "Internal error; cannot find value"
  Just rs -> case flip runRand gen <$> rs of
    Nothing -> End
    Just (a, g) -> Val a g

runMarkov :: (RandomGen g, Ord a) => Integer -> Markov g a -> g -> a -> Either String [a]
runMarkov n mkv gen x = go n
  where
    go m | m <= 0 = Right []
         | otherwise = (x:) <$> case runMarkov1 mkv gen x of
            Val a g -> runMarkov (n-1) mkv g a
            End -> Right []
            Error err -> Left err

fromMarkovI :: RandomGen g => MarkovI a -> Markov g a
fromMarkovI = Markov . M.map (fromList <$>)

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
insertTextInto mkv t    = case wds of 
    [wd] -> insertEnd wd mkv
    _    -> insertMkvPairsInto mkv (zip <*> tail $ wds)
  where wds = map clean $ filter (not . garbage) $ T.words t
        garbage t = any ($t)
          [T.null, flip elem "-[]':(){}\"*!# " . T.head, flip elem "[]':(){}\"*!# " . T.last, T.isPrefixOf "Chorus"]
        clean = T.filter (`notElem` "[]:(){}\"<>\\/ :")

fromTexts :: [T.Text] -> MarkovI T.Text
fromTexts = foldl' insertTextInto M.empty
