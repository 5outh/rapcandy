{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, BangPatterns, RecordWildCards #-}
module Markov where

import qualified Data.Map as M
import Control.Monad.Random
import Data.List(group, groupBy, nub, isPrefixOf)
import Control.Arrow
import Control.Applicative
import Data.Monoid
import Data.Ord
import Data.Function
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import System.Directory

newtype Markov g a = Markov{ getMarkov :: M.Map a (Maybe (Rand g a)) }

type MarkovI a = M.Map a (Maybe [(a, Rational)])

data Outcome g a = 
    Error String
  | Val a g 
  | End
    deriving (Show, Eq)

runMarkov1 :: (RandomGen g, Ord a, Show a) => Markov g a -> g -> a -> Outcome g a
runMarkov1 mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Error $ "Could not find value; internal error: " <> show x
  Just rs -> case flip runRand gen <$> rs of
    Nothing -> End
    Just (a, g) -> Val a g

runMarkov :: (RandomGen g, Ord a, Show a, Num n, Ord n) => n -> Markov g a -> g -> a -> Either String [a]
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

joinMarkovI :: Ord a => MarkovI a -> MarkovI a -> MarkovI a
joinMarkovI m1 m2 = foldr go m1 ks
  where ks = M.keys m2
        m1Dist k = join $ M.lookup k m1
        m2Dist k = join $ M.lookup k m2
        joined k = joinDistsMaybe (m1Dist k) (m2Dist k)
        go k = M.insert k (joined k)

joinDists :: (Eq k, Num v) => [(k, v)] -> [(k, v)] -> [(k, v)]
joinDists d1 d2 = map addSnds $ groupBy ((==) `on` fst) (d1 ++ d2)
  where addSnds xs = (fst $ head xs, sum $ map snd xs)

joinDistsMaybe :: (Eq k, Num v) => Maybe [(k, v)] -> Maybe [(k, v)] -> Maybe [(k, v)]
joinDistsMaybe mm1 mm2 = case (mm1, mm2) of
  (Nothing, Nothing) -> Nothing
  (m1, Nothing) -> m1
  (Nothing, m2) -> m2
  (Just m1, Just m2) -> Just $ joinDists m1 m2

markov :: (RandomGen g, Ord a) => [a] -> Markov g a
markov = Markov . M.map (fmap fromList) . markovi 

fromMarkovI = Markov . M.map (fromList <$>)
markoviString = markovi . map clean . filter notGarbage . T.words
markoviStrings = foldr1 joinMarkovI . map markoviString

markovString :: RandomGen g => T.Text -> Markov g T.Text
markovString = fromMarkovI . markoviString

markovStrings :: RandomGen g => [T.Text] -> Markov g T.Text
markovStrings = fromMarkovI . foldr1 joinMarkovI . map markoviString

getLines file = 
  filter (\x -> not (null x) && not ("[" `isPrefixOf` x)) . lines <$> readFile file

getDirectoryFiles file = filter (`notElem` [".", ".."]) <$> getDirectoryContents file

clean = T.filter (`notElem` "[]:(){}\"<>")

notGarbage t =
     T.head t `notElem` "-[]':(){}\"*"
  && T.last t `notElem` "[]':(){}\"*"
  && not (T.isPrefixOf "Chorus" t)

shead []    = Nothing
shead (x:_) = Just x

data GoState = GoState
  { mkv :: Markov StdGen T.Text
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
    g    <- newStdGen
    let res = T.unwords <$> runMarkov 25 mkv g seed
    case res of
      Left err  -> error err
      Right str -> go $ gs { lst = acc, acc = acc <> if start then "" else "\n" <> str, start = False }

randomTweet :: IO T.Text
randomTweet = do
  songs <- fmap ("./scraper/lyrics/" <>) <$> getDirectoryFiles "./scraper/lyrics"
  files <- replicateM 30 (uniform songs)
  lns <- (map T.pack . concat) <$> mapM getLines files
  let hds  = mapMaybe (shead . map clean . filter notGarbage . T.words) lns
      !mkv = markovStrings lns
  go $ GoState mkv hds "" "" True

test :: IO ()
test = do
  songs <- fmap ("./scraper/lyrics/" <>) <$> getDirectoryFiles "./scraper/lyrics"
  files <- replicateM 50 (uniform songs)
  !lns <- (map T.pack . concat) <$> mapM getLines files
  let hds  = mapMaybe (shead . map clean . filter notGarbage . T.words) lns
      !mkv = markovStrings lns
  putStrLn "loaded!"
  ress <- replicateM 100 $ go $ GoState mkv hds "" "" True
  forM_ ress $ \res -> do
    putStrLn $ T.unpack res
    putStrLn ""
