import Control.Arrow
import Data.List
import Common

main = interact $ show . solve . parse_lines . lines

solve inputs = total_ribbon inputs

total_ribbon inputs = process_presents inputs ribbon_needed
ribbon_needed dims = volume(dims) + smallest_perimeter(sort dims)

smallest_perimeter (a:b:_) = 2 * (a + b)
volume [l,w,h] = l * w * h
