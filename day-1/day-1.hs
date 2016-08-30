import Control.Arrow

main = interact $ show . solve

solve ('(':cs) = 1 + solve cs
solve (')':cs) = -1 + solve cs
solve [] = 0

