import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust)
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack, unpack)

main :: IO()
main = interact $ show . solve

solve :: (Num a, Enum a, Show a) => String -> a
solve input = find' (matchHash input matchTarget) [0..]

find' :: Foldable t => (a -> Bool) -> t a -> a
find' f l = fromJust $ find f l

matchTarget :: String
matchTarget = "00000"

matchHash :: Show a => String -> String -> a -> Bool
matchHash key matchStr val = isPrefixOf matchStr $ hashValue key val

md5str :: String -> String
md5str = show . md5 . pack

hashValue :: Show a => String -> a -> String
hashValue key val = md5str $ key ++ (show val)
