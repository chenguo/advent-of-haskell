import Data.List (find)
import Data.Maybe (fromJust)
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack, unpack)

main :: IO()
main = interact $ show . solve

solve :: (Num a, Enum a, Show a) => String -> a
solve input = fromJust $ find (matchHash input matchTarget) [0..]

matchTarget :: String
matchTarget = "000000"

matchHash :: Show a => String -> String -> a -> Bool
matchHash key matchStr val = prefixMatch matchStr $ hashValue key val

-- check if arg1 is prefix of arg2
prefixMatch :: String -> String -> Bool
prefixMatch [] _ = True
prefixMatch _ [] = False
prefixMatch (c1:cs1) (c2:cs2)
  | c1 == c2 = prefixMatch cs1 cs2
  | otherwise = False

md5str :: String -> String
md5str = show . md5 . pack

hashValue :: Show a => String -> a -> String
hashValue key val = md5str $ key ++ (show val)
