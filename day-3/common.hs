module Common (
  moves,
    coordSet
  )
where
import qualified Data.Set as Set

moves :: (Num a, Num b) => String -> [(a, b)]
moves = map translateMove

translateMove :: (Num a, Num b) => Char -> (a, b)
translateMove '^' = (0, 1)
translateMove 'v' = (0, -1)
translateMove '<' = (-1, 0)
translateMove '>' = (1, 0)
translateMove _ = (0, 0)

coordSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
coordSum (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

coordSet :: (Num a, Ord a, Num b, Ord b) => [(a, b)] -> Set.Set(a, b)
coordSet = buildCoordSet (0,0)

buildCoordSet :: (Num a, Ord a, Num b, Ord b) => (a, b) -> [(a, b)] -> Set.Set(a, b)
buildCoordSet loc (c:cs) = Set.insert newLoc (buildCoordSet newLoc cs)
  where newLoc = coordSum loc c
buildCoordSet _ [] = Set.singleton (0,0)
