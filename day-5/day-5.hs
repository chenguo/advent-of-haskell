import Common (vowelCounter, duplicateCounter, blacklist, state, consume, TextState)

main :: IO()
main = interact $ show . solve . lines

solve :: [String] -> Int
solve = length . filter checkState

combinedState vowels dupes blacklist = (state vowels)
                                       && (state dupes)
                                       && (state blacklist)
checkState = checkState' (vowelCounter 3) (duplicateCounter 2) (blacklist 2 ["ab", "cd", "pq", "xy"])

checkState' vowels dupes blacklist [] = combinedState vowels dupes blacklist
checkState' vowels dupes blacklist (c:cs) =
  checkState' vowels' dupes' blacklist' cs
  where vowels' = consume c vowels
        dupes' = consume c dupes
        blacklist' = consume c blacklist
