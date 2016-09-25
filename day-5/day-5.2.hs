import Common (pairDupes, spacedDupes, state, consume)

main :: IO()
main = interact $ show . solve . lines

solve :: [String] -> Int
solve = length . filter checkState

combinedState pairDupes spacedDupes = (state pairDupes) && (state spacedDupes)
checkState = checkState' (pairDupes 2) (spacedDupes 1)

checkState' pairDupes spacedDupes [] = combinedState pairDupes spacedDupes
checkState' pairDupes spacedDupes (c:cs) =
  checkState' pairDupes' spacedDupes' cs
  where pairDupes' = consume c pairDupes
        spacedDupes' = consume c spacedDupes
