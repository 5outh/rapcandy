{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
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

clean :: T.Text -> T.Text
clean = T.filter (`notElem` "[]:(){}\"<>\\/")

notGarbage :: T.Text -> Bool
notGarbage "" = False
notGarbage t =
     T.head t `notElem` "-[]':(){}\"*!#"
  && T.last t `notElem` "[]':(){}\"*!#"
  && not (T.isPrefixOf "Chorus" t)