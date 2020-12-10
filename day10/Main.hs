module Main where

import Data.List (sort)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap as M

oneDiffs :: [Int] -> Int
oneDiffs [] = 1
oneDiffs [x] = 1
oneDiffs (x:y:ys) | y - x == 1 = 1 + oneDiffs (y:ys)
                  | otherwise  = oneDiffs (y:ys)

threeDiffs :: [Int] -> Int
threeDiffs [] = 1
threeDiffs [x] = 1
threeDiffs (x:y:ys) | y - x == 3 = 1 + threeDiffs (y:ys)
                  | otherwise  = threeDiffs (y:ys)

-- Paths gives you a map from a joltage, to the amount of paths going to that joltage
paths :: IntSet -> IntMap Int
paths set = m
  where m = M.fromSet pathsFunc set
        pathsFunc :: (Int -> Int)
        pathsFunc i = if i == S.findMin set then 1 else sum [ M.findWithDefault 0 (i - diff) m | diff <- [1,2,3]]

main :: IO ()
main = do
    file <- readFile "input.txt"
    let nums = 0 : (sort $ read <$> lines file :: [Int])
    let set = S.fromAscList nums
    print (threeDiffs nums * oneDiffs nums)
    print (paths set !? S.findMax set)