import qualified Data.Set as Set
import qualified Common as Common

main = interact $ show . solve

solve :: String -> Int
solve = combinedUnique . Common.moves

combinedUnique :: (Num a, Ord a, Num b, Ord b) => [(a, b)] -> Int
combinedUnique ms = Set.size (Set.union (Common.coordSet m1) (Common.coordSet m2))
  where (m1,m2) = splitMoves ms

splitMoves :: (Num a, Num b) => [(a, b)] -> ([(a, b)], [(a, b)])
splitMoves (c1:c2:cs) = (c1 : (fst moves), c2 : (snd moves))
  where moves = splitMoves cs
splitMoves [c] = ([c], [])
splitMoves [] = ([], [])
