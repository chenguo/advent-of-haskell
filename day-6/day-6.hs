import Bullshit

updateGrid :: MapGrid -> String -> MapGrid
updateGrid grid lines = applyCommand grid $ parseLine lines

initGrid :: MapGrid
initGrid = mapGrid 1000 1000

solve = count . foldl updateGrid initGrid

main :: IO()
main = interact $ show . solve . lines
