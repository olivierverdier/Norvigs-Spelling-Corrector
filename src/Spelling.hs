module Spelling  where
-- module Spelling where

-- import qualified Data.ByteString.Char8            as B
import qualified Data.Text            as B
import qualified Data.Text.IO as TIO
import           Data.Char                        (isAlpha, toLower)
import           Data.List                        (foldl')
import qualified Data.Map.Strict                  as M
-- import           Data.Ord                         (comparing)
import qualified Data.Set                         as S
import           Paths_Norvigs_Spelling_Corrector (getDataFileName)
import Data.List (intersect, inits, tails)
import Control.Monad ((<=<))

-- import Control.Monad.State (get, put, runState, State)

import Control.Applicative ((<$>))

import Data.Monoid ((<>))

import First

type WordSet = S.Set String
type TrainingDict = M.Map String Int


transpose :: [a] -> [[a]]
transpose [] = []
transpose [_] = []
transpose (a:b:xs) = [b:a:xs]

replace :: [a] -> [a] -> [[a]]
replace _ [] = []
replace candidates (_:xs) = insert candidates xs

delete :: [a] -> [[a]]
delete [] = []
delete (_:xs) = [xs]

insert :: [a] -> [a] -> [[a]]
insert candidates l = (:l) <$> candidates


mkFull :: [a] -> ([a] -> [[a]]) -> ([a] -> [[a]])
mkFull begin editor l = (begin ++) <$> (editor l)

splits :: [a] -> [([a],[a])]
-- splits word = [ splitAt n word | n <- [0 .. length word] ]
splits word = zip (inits word) (tails word)

editsOnceWith :: [a] -> [a] -> [[a]]
editsOnceWith letters word = do
    (begin,end) <- splits word
    editor <- mkFull begin <$> [transpose, replace letters, delete, insert letters]
    editor end

known :: (Eq k) => M.Map k v -> [k] -> [k]
known dict ws = intersect ws (M.keys dict)

choices :: ([a] -> [a]) -> (a -> [a]) -> a -> [a]
choices inDict edits1 word = getFirst $ 
  mkFirst (\ w -> [w]) 
  <>  mkFirst edits1 
  <>  mkFirst (edits1 <=< edits1) 
  <> First [word]
  where
    mkFirst edit = First . inDict . edit $ word

chooseBest :: (Ord k) => k -> M.Map k Int -> [k] -> k
chooseBest zero dict ws = 
    fst $
      foldl (\ (word,score) w -> let newScore = getScore $ M.lookup w dict in if newScore > score then (w,newScore) else (word,score) ) (zero,0) ws
  where
    getScore :: Maybe Int -> Int
    getScore Nothing = 0
    getScore (Just i) = i

{- Getting the training dictionary -}

nWords :: IO TrainingDict
nWords = do
  ws <- getDataFileName "big.txt" >>= TIO.readFile
  return (train . lowerWords . B.unpack $ ws)

lowerWords :: String -> [String]
lowerWords = words . map normalize
  where normalize c = if isAlpha c then toLower c else ' '

train :: (Ord k, Num v) => [k] -> M.Map k v
train = foldl' (\ dict x -> M.insertWith (+) x 1 dict) M.empty

{- Piecing all together -}

alphabet :: String
alphabet = ['a' .. 'z']

correct :: M.Map String Int -> String -> String
correct dict word = chooseBest "" dict (choices (known dict) (editsOnceWith alphabet) word)

ioCorrect :: String -> IO String
ioCorrect word =  flip correct word <$> nWords


-- edits1 :: String -> WordSet
-- edits1 w = S.fromList $ deletes ++ transposes ++ replaces ++ inserts
--   where
--     splits = [ splitAt n w | n <- [0 .. length w - 1] ]
--     deletes = map (\(a, b) -> a ++ tail b) splits
--     transposes = [ a ++ [b1, b0] ++ bs
--                  | (a, b0:b1:bs) <- splits ]
--     replaces = [ as ++ [c] ++ bs
--                | (as, _:bs) <- splits, c <- alphabet]
--     inserts = [ a ++ [c] ++ b
--               | (a,b) <- splits, c <- alphabet]


-- edits2 :: String -> WordSet
-- edits2 = S.unions . S.toList . S.map edits1 . edits1
-- 
-- knownEdits2 :: String -> TrainingDict -> WordSet
-- knownEdits2 w nwords = edits2 w `S.intersection` M.keysSet nwords
-- 
-- known :: WordSet -> TrainingDict -> WordSet
-- known inputSet nwords = inputSet `S.intersection` M.keysSet nwords

-- choices :: String -> TrainingDict -> WordSet
-- choices w ws = foldr orNextIfEmpty (S.singleton w)
--   [ known (S.singleton w) ws
--   , known (edits1 w) ws
--   , knownEdits2 w ws
--   ]
--   where orNextIfEmpty x y = if S.null x then y else x

-- choices :: TrainingDict -> String -> [String]

-- chooseBest :: WordSet -> TrainingDict -> String
-- chooseBest ch ws = chooseBest' $
--   ws `M.intersection` M.fromList (map (\x -> (x, ())) (S.toList ch))
--   where
--     chooseBest' bestChs = head (map fst (sortCandidates bestChs))
--     sortCandidates = sortBy (comparing snd) . M.toList
-- 
-- correct :: TrainingDict -> String -> String
-- correct ws w = chooseBest (choices w ws) ws
-- 

-- 
--
-- Fold with State...
--
-- trainOnce :: (Ord k, Num v) => k -> State (M.Map k v) ()
-- trainOnce x = do
--     m <- get
--     put $ M.insertWith (+) x 1 m
-- 
-- trainAll :: (Ord k, Num v) => [k] -> M.Map k v
-- trainAll words = snd $ runState (mapM trainOnce words) M.empty
