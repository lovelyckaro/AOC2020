module Main where

import Data.List (inits, tails, find, sort)

twoSum :: [Int] -> Int -> Bool
twoSum xs x = not $ null [(n1, n2) | n1 <- xs, n2 <- xs, n1 > n2, n1 + n2 == x]

extractPreamble :: [Int] -> Int -> [Int]
extractPreamble all index = take 25 (drop (index - 25) all)

testNumber :: [Int] -> Int -> Bool
testNumber all index = twoSum (extractPreamble all index) (all !! index)

findFirstFail all = (all !!) <$> find (not . testNumber all) [25 .. length all - 1]


myNum :: Int
myNum = 507622668

cSubsequences = filter (not . null) . concatMap inits . tails

findRange :: [Int] -> Int -> Maybe [Int]
findRange all sumTo = find (\xs -> sum xs == sumTo && length xs > 1) (cSubsequences all)

main :: IO ()
main = do
    file <- readFile "input.txt"
    let nums = read <$> lines file :: [Int]
    print (findFirstFail nums)
    print(sort <$> findRange nums myNum)
    return ()
