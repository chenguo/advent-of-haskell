module Common (
  vowelCounter,
  duplicateCounter,
  blacklist,
  pairDupes,
  state,
  consume
) where

import Data.Maybe (fromJust)
import Data.Tuple
import qualified Data.Map as Map

class TextState a where
  consume :: Char -> a -> a
  state :: a -> Bool
  updateState :: a -> a

-- Count vowels
data VowelCounter = VowelCounter Int Bool
vowelCounter n = VowelCounter n False

instance TextState VowelCounter where
  consume c counter
    | state counter     = counter
    | isVowel c         = updateState counter
    | otherwise         = counter
  state (VowelCounter _ st) = st
  updateState (VowelCounter n _)
    | n <= 1    = VowelCounter 0 True
    | otherwise = VowelCounter (n - 1) False

isVowel :: Char -> Bool
isVowel c
  | c == 'a'  = True
  | c == 'e'  = True
  | c == 'i'  = True
  | c == 'o'  = True
  | c == 'u'  = True
  | otherwise = False

-- Looks for runs of duplicate letters
data DuplicateCounter = DuplicateCounter Char Int Int Bool
duplicateCounter n = DuplicateCounter '\0' n 1 False

instance TextState DuplicateCounter where
  consume c counter
    | state counter         = counter
    | c == dupeChar counter = updateState counter
    | otherwise             = reset c counter
  state (DuplicateCounter _ _ _ st) = st
  updateState (DuplicateCounter c n i st)
    | i' >= n   = DuplicateCounter c n n True
    | otherwise = DuplicateCounter c n i' False
    where i' = i + 1

dupeChar :: DuplicateCounter -> Char
dupeChar (DuplicateCounter c _ _ _) = c

reset :: Char -> DuplicateCounter -> DuplicateCounter
reset c (DuplicateCounter _ n _ _) = DuplicateCounter c n 1 False

-- Blacklist strings of characters
data Blacklist = Blacklist Int [String] String Bool
blacklist n strs = Blacklist n strs [] True

instance TextState Blacklist where
  consume c (Blacklist n strs cs st)
    | st == False = Blacklist n' strs cs' st
    | otherwise   = updateState $ Blacklist n' strs cs' st
    where cs' = updateCharRun n cs c
          n' = updateCharN n
  state (Blacklist _ _ _ st) = st
  updateState (Blacklist n strs cs False) = Blacklist n strs cs False
  updateState (Blacklist n strs cs _) = Blacklist n strs cs (not (any (== cs) strs))


updateCharRun :: Int -> [Char] -> Char -> [Char]
updateCharRun 0 cs c = (tail cs) ++ [c]
updateCharRun _ cs c = cs ++ [c]

updateCharN :: Int -> Int
updateCharN 0 = 0
updateCharN n = n - 1


-- Non-overlapping pair duplicates
data DupeRuns = DupeRuns Int Int (Map.Map String Int) String Bool
pairDupes n = DupeRuns n (-1 * n) Map.empty "" False

instance TextState DupeRuns where
  consume c (DupeRuns n pos m cs st)
    | st == True = DupeRuns n pos' m cs' st
    | otherwise  = updateState $ DupeRuns n pos' m cs' st
    where pos' = pos + 1
          cs' = updateCharRun n cs c
  state (DupeRuns _ _ _ _ st) = st
  updateState (DupeRuns n pos m cs st)
    | hasDupeRun n pos m cs = DupeRuns n pos m cs st
    | pos >= 0 =
        let m' = Map.insert cs pos m
        in DupeRuns n pos m' cs st
    | otherwise = DupeRuns n pos m cs st

hasDupeRun :: Int -> Int -> (Map.Map String Int) -> String -> Bool
hasDupeRun n pos m cs
  | pos < 0         = False
  | Map.member cs m = (fromJust (Map.lookup cs m)) > pos - n
  | otherwise       = False
