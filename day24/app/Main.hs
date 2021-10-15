{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Test.QuickCheck (Arbitrary (arbitrary), elements, quickCheck)

data HexCoord = HexCoord {x :: Int, y :: Int, z :: Int}
  deriving (Show, Eq, Ord)

instance Semigroup HexCoord where
  (HexCoord x1 y1 z1) <> (HexCoord x2 y2 z2) = HexCoord (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid HexCoord where
  mempty = HexCoord 0 0 0

instance Arbitrary HexCoord where
  arbitrary = dirsToCoord <$> arbitrary

data Direction = East | West | SouthEast | SouthWest | NorthEast | NorthWest
  deriving (Show, Enum)

instance Arbitrary Direction where
  arbitrary = elements [East .. NorthWest]

data Color = Black | White
  deriving (Show, Eq)

propHexCoords :: HexCoord -> Bool
propHexCoords (HexCoord {..}) = x + y + z == 0

dirsToCoord :: [Direction] -> HexCoord
dirsToCoord [] = mempty
dirsToCoord (East : xs) = HexCoord 1 (-1) 0 <> dirsToCoord xs
dirsToCoord (West : xs) = HexCoord (-1) 1 0 <> dirsToCoord xs
dirsToCoord (SouthEast : xs) = HexCoord 0 (-1) 1 <> dirsToCoord xs
dirsToCoord (SouthWest : xs) = HexCoord (-1) 0 1 <> dirsToCoord xs
dirsToCoord (NorthEast : xs) = HexCoord 1 0 (-1) <> dirsToCoord xs
dirsToCoord (NorthWest : xs) = HexCoord 0 1 (-1) <> dirsToCoord xs

readDirections :: String -> [Direction]
readDirections [] = []
readDirections ('e' : xs) = East : readDirections xs
readDirections ('w' : xs) = West : readDirections xs
readDirections ('s' : 'e' : xs) = SouthEast : readDirections xs
readDirections ('s' : 'w' : xs) = SouthWest : readDirections xs
readDirections ('n' : 'e' : xs) = NorthEast : readDirections xs
readDirections ('n' : 'w' : xs) = NorthWest : readDirections xs

getCoord :: String -> HexCoord
getCoord = dirsToCoord . readDirections

count :: [HexCoord] -> Map HexCoord Int
count = foldr (\k m -> M.insertWith (+) k 1 m) M.empty

color :: Map HexCoord Int -> Map HexCoord Color
color = M.map (\x -> if even x then White else Black)

countBlackTiles :: Map HexCoord Color -> Int
countBlackTiles = M.size . M.filter (== Black)

neighbors :: HexCoord -> [HexCoord]
neighbors p = [p <> dirsToCoord [dir] | dir <- [East .. NorthWest]]

propAmountOfNeighbors :: HexCoord -> Bool
propAmountOfNeighbors p = length (neighbors p) == 6

propValidNeighbors :: HexCoord -> Bool
propValidNeighbors p = all propHexCoords (neighbors p)

tick :: Map HexCoord Color -> Map HexCoord Color
tick current = new
  where
    new = M.fromSet getTile (S.fromList (concatMap neighbors $ M.keys current))
    getTile :: HexCoord -> Color
    getTile coord = case M.lookup coord current of
      Nothing -> neighborsToNewColor (map (`M.lookup` current) (neighbors coord)) White
      (Just color) -> neighborsToNewColor (map (`M.lookup` current) (neighbors coord)) color

tickNTimes :: Int -> Map HexCoord Color -> Map HexCoord Color
tickNTimes 0 current = current
tickNTimes n current = tickNTimes (n - 1) (tick current)

neighborsToNewColor :: [Maybe Color] -> Color -> Color
neighborsToNewColor xs = amountToColor (countBlack xs)
  where
    countBlack :: [Maybe Color] -> Int
    countBlack [] = 0
    countBlack (Nothing : xs) = countBlack xs
    countBlack (Just White : xs) = countBlack xs
    countBlack (Just Black : xs) = 1 + countBlack xs
    amountToColor :: Int -> Color -> Color
    amountToColor 0 Black = White
    amountToColor 1 Black = Black
    amountToColor 2 Black = Black
    amountToColor n Black = White
    amountToColor 2 White = Black
    amountToColor n White = White

part1 :: String -> Int
part1 = countBlackTiles . color . count . map getCoord . lines

part2 :: String -> Int
part2 = countBlackTiles . tickNTimes 100 . color . count . map getCoord . lines

main :: IO ()
main = do
  inp <- readFile "input"
  print ("Part1: " <> show (part1 inp))
  print ("Part2: " <> show (part2 inp))

tests :: IO ()
tests = do
  quickCheck propHexCoords
  quickCheck propAmountOfNeighbors
  quickCheck propValidNeighbors
  example <- readFile "example"
  quickCheck (part1 example == 10)
  quickCheck (part2 example == 2208)
