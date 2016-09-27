-- One single file cause I can't figure out how to import and
-- export data constructors. Sigh :(
module Bullshit (
  parseLine,
  applyCommand,
  Bullshit.count,
  StateGrid,
  MapGrid,
  mapGrid
  ) where

-- state-grid.hs imports--

import qualified Data.Primitive.ByteArray as Array
import qualified Data.Vector.Primitive.Mutable as Vector
import qualified Data.Map.Strict as Map
import Data.Bool
import Data.Maybe

-- parse.hs imports --
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


-- state-grid.hs code --

data Op = ON | OFF | TOGGLE | NOOP deriving (Show, Eq)
data Coord = Coord Int Int deriving (Show, Ord, Eq)
data Command = Command Op Coord Coord deriving (Show)

class StateGrid a where
  applyCommand :: a -> Command -> a
  count :: a -> Int

type CoordStateMap = Map.Map Coord Int
data MapGrid = MapGrid Int Int (Map.Map Coord Int) deriving (Show)
mapGrid maxX maxY = MapGrid maxX maxY Map.empty

instance StateGrid MapGrid where
  applyCommand grid@(MapGrid maxX maxY m) c@(Command op (Coord x1 y1) (Coord x2 y2))
    -- What's the best way to share code + structure here?
    -- | op == ON     = MapGrid maxX maxY $ boxOn m x1 x2 y1 y2
    -- | op == OFF    = MapGrid maxX maxY $ boxOff m x1 x2 y1 y2
    -- | op == TOGGLE = MapGrid maxX maxY $ boxToggle m x1 x2 y1 y2
    | op == ON     = MapGrid maxX maxY $ boxAdd 1 m x1 x2 y1 y2
    | op == OFF    = MapGrid maxX maxY $ boxAdd (-1) m x1 x2 y1 y2
    | op == TOGGLE = MapGrid maxX maxY $ boxAdd 2 m x1 x2 y1 y2
    | otherwise    = grid
  -- count (MapGrid _ _ m) = Map.size $ Map.filter (\x -> x /= 0) m
  count (MapGrid _ _ m) = Map.foldl (+) 0 m

type MapOpFn = CoordStateMap -> Coord -> CoordStateMap

pointOn :: MapOpFn
pointOn m coord = Map.insert coord 1 m

pointOff :: MapOpFn
pointOff m coord = Map.insert coord 0 m

pointToggle :: MapOpFn
pointToggle m coord = Map.insertWith pseudoXor coord 1 m

pseudoXor :: Int -> Int -> Int
pseudoXor a b
  | a /= b = 1
  | otherwise = 0

pointAdd :: Int -> MapOpFn
pointAdd n m coord
  | Map.member coord m = Map.insertWith flooredSum coord n m
  | n < 0              = m
  | otherwise          = Map.insert coord n m

flooredSum :: Int -> Int -> Int
flooredSum a b
  | a + b >= 0 = a + b
  | otherwise = 0

applyOpToRow :: MapOpFn -> CoordStateMap -> Int -> Int -> Int -> CoordStateMap
applyOpToRow fn m x y1 y2 =
  let opFn = \m y -> fn m $ Coord x y
  in foldl opFn m [y1..y2]

applyOpToBox :: MapOpFn -> CoordStateMap -> Int -> Int -> Int -> Int -> CoordStateMap
applyOpToBox fn m x1 x2 y1 y2 =
  let rowFn = \m x -> applyOpToRow fn m x y1 y2
  in foldl rowFn m [x1..x2]

boxOn :: CoordStateMap -> Int -> Int -> Int -> Int -> CoordStateMap
boxOn = applyOpToBox pointOn

boxOff :: CoordStateMap -> Int -> Int -> Int -> Int -> CoordStateMap
boxOff = applyOpToBox pointOff

boxToggle :: CoordStateMap -> Int -> Int -> Int -> Int -> CoordStateMap
boxToggle = applyOpToBox pointToggle

boxAdd :: Int -> CoordStateMap -> Int -> Int -> Int -> Int -> CoordStateMap
boxAdd n = applyOpToBox $ pointAdd n


-- How the fuck do you use this shit?
-- data VectorGrid = VectorGrid Int Int (Vector.MVector (PrimState m) s)
-- vectorGrid maxX maxY = VectorGrid maxX maxY (Vector.MVector (maxX * maxY))


-- parse.hs code --

def = emptyDef {
  reservedNames = ["on", "off", "toggle", "through"]
  }
TokenParser{ reserved = m_reserved } = makeTokenParser def

parseOp :: Parser Op
parseOp = (m_reserved "turn on" >> return ON)
  <|> (m_reserved "turn off" >> return OFF)
  <|> (m_reserved "toggle" >> return TOGGLE)
  <|> return NOOP

parseCoord :: Parser Coord
parseCoord = do {
  x <- read <$> many digit;
  (char ',');
  y <- read <$> many digit;
  return $ Coord x y;
}

parseCommand :: Parser Command
parseCommand  = do {
  op <- parseOp;
  coord1 <- parseCoord;
  (string " through "); -- weird thing about leading space if I used m_reserved... ?????
  coord2 <- parseCoord;
  return $ Command op coord1 coord2
}

parseLine :: String -> Command
parseLine l =  case (parse parseCommand "" l) of
  Left err -> Command NOOP (Coord 0 0) (Coord 0 0)
  Right cmd -> cmd
