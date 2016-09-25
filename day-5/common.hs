module Common (
  vowelCounter,
  duplicateCounter,
  blacklist,
  pairDupes,
  spacedDupes,
  state,
  consume,
  TextState
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
    | st == False = Blacklist n strs cs' st
    | otherwise   = updateState $ Blacklist n strs cs' st
    where cs' = updateCharRun n cs c
  state (Blacklist _ _ _ st) = st
  updateState bl@(Blacklist n strs cs False) = bl
  updateState (Blacklist n strs cs _) = Blacklist n strs cs (not (any (== cs) strs))

updateCharRun :: Int -> [Char] -> Char -> [Char]
updateCharRun n cs c
  | n == length cs = (tail cs) ++ [c]
  | otherwise        = cs ++ [c]


-- Non-overlapping pair duplicates
data DupeRuns = DupeRuns Int Int (Map.Map String Int) String Bool
pairDupes n = DupeRuns n (-1 * n) Map.empty "" False

instance TextState DupeRuns where
  consume c dupes@(DupeRuns _ _ _ _ True) = dupes
  consume c (DupeRuns n pos m cs False) =
    let pos' = pos + 1
        cs' = updateCharRun n cs c
    in updateState (DupeRuns n pos' m cs' False)
  state (DupeRuns _ _ _ _ st) = st
  updateState dupes@(DupeRuns _ _ _ _ True) = dupes
  updateState (DupeRuns n pos m cs False) =
    DupeRuns n pos (addRunToMap m pos cs) cs (hasDupeRun n m pos cs)

hasDupeRun :: Int -> (Map.Map String Int) -> Int -> String -> Bool
hasDupeRun n m pos cs
  | pos < n         = False
  | Map.member cs m = (fromJust (Map.lookup cs m)) + n <= pos
  | otherwise       = False

addRunToMap :: (Map.Map String Int) -> Int -> String -> (Map.Map String Int)
addRunToMap m pos cs
  | Map.member cs m = m
  | pos >= 0        = Map.insert cs pos m
  | otherwise       = m


-- Duplicate letter with spacing
data SpacedDupes = SpacedDupes Int String Bool
spacedDupes n = SpacedDupes (n + 2) "" False

instance TextState SpacedDupes where
  consume c dupes@(SpacedDupes _ _ True) = dupes
  consume c (SpacedDupes n cs st) = updateState $ SpacedDupes n cs' st
    where cs' = updateCharRun n cs c
  state (SpacedDupes _ _ st) = st
  updateState (SpacedDupes n cs@(c0:c1:c2:[]) st) =
    SpacedDupes n cs (c0 == c2)
  updateState dupes = dupes
