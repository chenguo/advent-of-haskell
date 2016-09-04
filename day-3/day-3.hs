import Data.Tuple
import qualified Data.Set as Set
import qualified Common as Common

main = interact $ show . solve

solve = uniqCoords . Common.moves

uniqCoords :: (Num a, Ord a, Num b, Ord b) => [(a, b)] -> Int
uniqCoords = Set.size . Common.coordSet
