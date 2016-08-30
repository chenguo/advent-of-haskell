import Control.Arrow
import Common

main = interact $ show . solve . parse_lines . lines

solve inputs = total_wrapper inputs

total_wrapper inputs = process_presents inputs area_needed

area_needed [l,w,h] = sum_sides (l*w) (w*h) (l*h)
area_needed _ = 0
sum_sides a b c = 2 * (a + b + c) + minimum [a, b, c]
