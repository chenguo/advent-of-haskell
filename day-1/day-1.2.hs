import Control.Arrow

main = interact $ show . solve

solve cs = basement 0 0 cs

-- If we don't need to return 0 by default, we can drop
-- the pos argument and just add onto the recursive call
basement pos 0 (')':cs) = pos + 1
basement pos floor (')':cs) = basement (pos + 1) (floor - 1) cs
basement pos floor ('(':cs) = basement (pos + 1) (floor + 1) cs
basement _ _ [] = 0
