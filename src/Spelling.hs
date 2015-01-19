module Spelling  where

{- A few explicit Prelude imports -}

import Prelude ()

import GHC.Base ((.), ($))
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
import Control.Applicative ((<$>), liftA2)
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
insert letters l = (:l) <$> letters

replace :: [a] -> [a] -> [[a]]
replace _ [] = []
replace letters (_:xs) = insert letters xs

allEditors :: [a] -> [[a] -> [[a]]]
allEditors letters =  [return . transpose, replace letters, return . delete, insert letters]


splits :: [a] -> [([a],[a])]
splits word = zip (inits word) (tails word)

editsOnceWith :: [[a]->[[a]]] -> [a] -> [[a]]
editsOnceWith editors word = do
    (begin,end) <- splits word
    editor <- editors
    endedit <- editor end
    return $ begin ++ endedit


known :: (Eq k, Ord k) => M.Map k v -> [k] -> [(k,v)]
known dict = mapMaybe myLookup
    where
      myLookup w = -- returns either Nothing or Just (w,f) if w is found in dict
        (,) w <$> M.lookup w dict

choices :: ([a] -> [(a,b)]) -> (a -> [a]) -> a -> [(a,b)]
choices inDict edits1 word = getFirst $ 
  mkFirst return
  <>  mkFirst edits1 
  <>  mkFirst (edits1 <=< edits1) 
  where
    mkFirst edit = MkFirst . inDict . edit $ word

{- Missing function from the Data.Foldable package: maximumBy with default for
  empty list -}

maxByOrDefault :: (Foldable t) => (a -> a -> Ordering) -> a -> t a -> a
maxByOrDefault comp = foldl' (\ e e' -> if comp e' e == GT then e' else e)

{- Choose word with best score -}

chooseBest :: (Ord k, Ord v, Num v) => k -> [(k,v)] -> k
chooseBest nothing choices' = fst $ maxByOrDefault (comparing snd) (nothing, 0) choices'

{- Getting the training dictionary -}

nWords :: IO TrainingDict
nWords = do
  fileName <- getDataFileName "big.txt"
  ws <- readFile fileName
  return (train . lowerWords . unpack $ ws)

lowerWords :: String -> [String]
lowerWords = words . fmap normalize
  where normalize c = if isAlpha c then toLower c else ' '

train :: (Ord k, Num v) => [k] -> M.Map k v
train = foldl' (\ dict x -> M.insertWith (+) x 1 dict) M.empty

{- Piecing all together -}

alphabet :: String
alphabet = ['a' .. 'z']

correct :: TrainingDict -> String -> String
correct dict word = chooseBest "??" $ choices (known dict) (editsOnceWith $ allEditors alphabet) word

ioCorrect :: String -> IO String
ioCorrect =  liftA2 correct nWords . return
