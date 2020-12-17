module Main where

import qualified Data.Set as S
import Data.Set (Set, (\\), )
import Control.Monad (guard)

type Point = (Int, Int, Int, Int)
type World = Set Point

adjacent :: Point -> [Point]
adjacent (x,y,z,t) = do
    dx <- [-1, 0, 1]
    dy <- [-1, 0, 1]
    dz <- [-1, 0, 1]
    dt <- [-1, 0, 1]
    guard (not (dx == 0 && dy == 0 && dz == 0 && dt == 0))
    return (x + dx, y + dy, z + dz, t + dt)

countAdjacent :: World -> Point -> Int
countAdjacent w p = length $ filter (`S.member` w) (adjacent p)

tickPoint :: World -> Point -> Maybe Point
tickPoint w p = let neighbors = countAdjacent w p in
    if p `S.member` w 
        then case neighbors of
            2 -> Just p
            3 -> Just p
            _ -> Nothing
        else case neighbors of
            3 -> Just p
            _ -> Nothing

tick :: World -> World
tick world = foldr S.union S.empty $ S.map tickCurrent world
    where
        tickCurrent :: Point -> World
        tickCurrent p = maybe S.empty S.singleton (tickPoint world p) `S.union` tickNeighbors (adjacent p)
        tickNeighbors :: [Point] -> World
        tickNeighbors [] = S.empty
        tickNeighbors (p:ps) = 
            if p `S.member` world 
                then tickNeighbors ps
                else maybe S.empty S.singleton (tickPoint world p) `S.union` tickNeighbors ps


parseWorld :: [[Char]] -> World
parseWorld ls = S.fromList (filter valid indices)
    where indices = [(x,y,0,0) | x <- [0.. length (head ls) - 1], y <- [0..length ls - 1]]
          valid :: Point -> Bool
          valid (x,y,_,_) = ((ls !! y) !! x) == '#'

run :: World -> [World]
run = iterate tick

main :: IO ()
main = do
    file <- readFile "input.txt"
    let initWorld = parseWorld (lines file)
    let futureWorlds = run initWorld
    print (S.size (futureWorlds !! 6))