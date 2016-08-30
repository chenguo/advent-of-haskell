module Common where

process_presents dims fn = foldl (+) 0 (map fn dims)

-- Parsing utilities
parse_lines lines = map parse lines
parse line = parse_helper "0" line

parse_helper digits ('x':cs) = parse_int(digits) : parse cs
parse_helper digits (c:cs) = parse_helper (digits ++ [c]) cs
parse_helper digits [] = [parse_int(digits)]

parse_int x = read x :: Int
