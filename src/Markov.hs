{-# LANGUAGE NoMonomorphismRestriction #-}
module Markov where

import qualified Data.Map as M
import Control.Monad.Random
import Data.List(group)
import Data.Maybe(listToMaybe)
import Control.Arrow
import Control.Applicative((<$>), (<*>))

newtype Markov g a = Markov{ getMarkov :: M.Map a (Maybe (Rand g a)) }

data Outcome g a = 
    Error String
  | Val a g 
  | End
    deriving (Show, Eq)

runMarkov1 :: (RandomGen g, Ord a) => Markov g a -> g -> a -> Outcome g a
runMarkov1 mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Error "Could not find value; internal error."
  Just rs -> case flip runRand gen <$> rs of
    Nothing -> End
    Just (a, g) -> Val a g

runMarkov :: (RandomGen g, Ord a) => Markov g a -> g -> a -> Either String [a]
runMarkov mkv gen x = (x:) <$> case runMarkov1 mkv gen x of
  Val a g -> runMarkov mkv g a
  End -> Right []
  Error err -> Left err 

rle :: Ord a => [a] -> [(a, Rational)]
rle = map (head &&& (fromIntegral . length)) . group

markov :: Ord a => [a] -> Markov StdGen a
markov xs = Markov 
            $ M.map (fmap fromList . wrapMaybe . rle) 
            $ foldr (uncurry accum) wordMap  withNexts
  where withNexts = zip xs (tail xs)
        wordMap = foldr (`M.insert` []) M.empty xs
        accum w nxt = M.adjust (nxt:) w
        wrapMaybe x = case x of { [] -> Nothing; xs -> Just xs }

markovString :: String -> Markov StdGen String
markovString = markov . words

testStr :: String
testStr = "I am going to the market today, oh yes I am."

test = do
  g <- newStdGen
  print $ runMarkov (markovString testStr) g "going"
