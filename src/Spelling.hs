module Spelling  where

{- A few explicit Prelude imports -}

import Prelude ()

import GHC.Base ((.))
import GHC.Num (Num, (+))
import GHC.Types (Int)
import Data.Tuple (fst, snd)
import System.IO (IO)
import Data.Eq (Eq, (==))
import Data.Functor (fmap)
import Data.Ord (Ord, Ordering(GT),
                comparing) -- not in Prelude
import Data.List ((++), zip,
                 inits, tails) -- not in Prelude
import Control.Monad (return,
                     (<=<)) -- not in Prelude

{- Other imports -}

import Data.String (String, words)
import Data.Char (isAlpha, toLower)
import Data.Text (unpack)
import Data.Text.IO (readFile)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Foldable (Foldable, foldl')

import Paths_Norvigs_Spelling_Corrector (getDataFileName)
import First (First(MkFirst), getFirst)

type TrainingDict = M.Map String Int


transpose :: [a] -> [a]
transpose [] = []
transpose [_] = []
transpose (a:b:xs) = b:a:xs

delete :: [a] -> [a]
delete [] = []
delete (_:xs) = xs

insert :: [a] -> [a] -> [[a]]
insert letters word = 
    do -- List
      l <- letters
      return (l:word)

replace :: [a] -> [a] -> [[a]]
replace _ [] = []
replace letters (_:xs) = insert letters xs

allEditors :: [a] -> [[a] -> [[a]]]
allEditors letters =  [return . transpose, replace letters, return . delete, insert letters]


splits :: [a] -> [([a],[a])]
splits = 
    do -- Reader
      iw <- inits
      tw <- tails
      return (zip iw tw)

editsOnceWith :: [[a]->[[a]]] -> [a] -> [[a]]
editsOnceWith editors word = do -- List
    (begin,end) <- splits word
    editor <- editors
    endedit <- editor end
    return (begin ++ endedit)


inDict :: (Eq k, Ord k) => M.Map k v -> [k] -> [(k,v)]
inDict dict = mapMaybe myLookup
    where
      myLookup w = -- returns either Nothing or Just (w,f) if w is found in dict (f is the frequency)
        -- (,) w <$> M.lookup w dict
        do -- Maybe
          f <- M.lookup w dict
          return (w,f)

allChoices :: ([a] -> [(a,b)]) -> (a -> [a]) -> a -> [(a,b)]
allChoices inDict' edits1 word = getFirst possibilities
  where
    possibilities = 
      mkFirst return
      <>  mkFirst edits1 
      <>  mkFirst (edits1 <=< edits1) 
    mkFirst edit = (MkFirst . inDict' . edit) word

{- Missing function from the Data.Foldable package: maximumBy with default for
  empty list -}

maxByOrDefault :: (Foldable t) => (a -> a -> Ordering) -> a -> t a -> a
maxByOrDefault comp def list = foldl' max' def list
  where
    max' e e' = if comp e' e == GT then e' else e

{- Choose word with best score -}

chooseBest :: (Ord k, Ord v, Num v) => k -> [(k,v)] -> k
chooseBest nothing choices' = fst bestPair
    where bestPair = maxByOrDefault (comparing snd) (nothing, 0) choices'

{- Getting the training dictionary -}

nWords :: IO TrainingDict
nWords = do -- IO
  fileName <- getDataFileName "big.txt"
  ws <- readFile fileName
  return ((train . lowerWords . unpack) ws)

lowerWords :: String -> [String]
lowerWords = words . fmap normalize
  where normalize c = if isAlpha c then toLower c else ' '

train :: (Ord k, Num v) => [k] -> M.Map k v
train trainWords = foldl' increment M.empty trainWords
  where
    increment dict x = M.insertWith (+) x 1 dict

{- Piecing all together -}

alphabet :: String
alphabet = ['a' .. 'z']

correct :: TrainingDict -> String -> String -> String
correct dict notfound word = chooseBest notfound choices
  where
    choices = allChoices (inDict dict) (editsOnceWith (allEditors alphabet)) word

ioCorrect :: String -> IO String
ioCorrect w =  do -- IO
    d <- nWords
    return (correct d "" w)
