{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (transpose)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    errorBundlePretty,
    parse,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, eol, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Tile = Tile {north, south, east, west :: Side, tileId :: TileId}
  deriving (Show)

newtype Side = Side String
  deriving (Show)

-- Important! Sides can be flipped, and should be equal when comparing flipped and unflipped versions.
instance Eq Side where
  (Side xs) == (Side ys) = xs == ys || xs == reverse ys

instance Hashable Side where
  hashWithSalt n (Side xs) = hashWithSalt n xs + hashWithSalt n (reverse xs)

type Parser = Parsec Void String

pTiles :: Parser [Tile]
pTiles = some pTile

pTile :: Parser Tile
pTile = do
  tileId <- string "Tile" >> space >> decimal
  string ":"
  eol
  (north, south, east, west) <- pSides
  eol
  return $ Tile {..}

pSides :: Parser (Side, Side, Side, Side)
pSides = do
  xs <- pMaze
  let north = head xs
  let south = last xs
  let ts = transpose xs
  let west = head ts
  let east = last ts
  return (Side north, Side south, Side east, Side west)

pMaze :: Parser [String]
pMaze = some (some (char '.' <|> char '#') >>= \t -> eol >> return t)

type Frequency = Int

type TileId = Int

sides :: Tile -> [Side]
sides (Tile {..}) = [north, south, east, west]

countFreqs :: [Tile] -> HashMap Side (Frequency, [TileId])
countFreqs tiles = foldr inserter M.empty [(side, tileId tile) | tile <- tiles, side <- sides tile]
  where
    inserter :: (Side, TileId) -> HashMap Side (Frequency, [TileId]) -> HashMap Side (Frequency, [TileId])
    inserter (s, i) = M.insertWith adder s (1, [i])
    adder :: (Frequency, [TileId]) -> (Frequency, [TileId]) -> (Frequency, [TileId])
    adder (f1, ts1) (f2, ts2) = (f1 + f2, ts1 <> ts2)

uniques :: HashMap Side (Frequency, [TileId]) -> HashMap TileId [Side]
uniques = M.foldrWithKey (\k (f, i : rest) xs -> M.insertWith (<>) i [k] xs) M.empty . M.filter (\(f, i) -> f == 1)

main :: IO ()
main = do
  inp <- readFile "input"
  xs <- case parse pTiles "input" inp of
    Right xs -> return xs
    Left error -> putStr (errorBundlePretty error) >> return []
  print . product . M.keys . M.filter ((== 2) . length) . uniques . countFreqs $ xs
  return ()
