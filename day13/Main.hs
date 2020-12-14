module Main where

import Data.List (sort)

replace :: Char -> Char
replace ',' = ' '
replace c = c

toBus :: String -> Maybe Int
toBus "x" = Nothing
toBus bus = Just $ read bus

waitingTime :: Int -> Int -> Int
waitingTime time bus = ((time `div` bus) + 1) * bus - time

actualBuses :: Int -> [Maybe Int] -> [(Int, Int)]
actualBuses _ [] = []
actualBuses index (Just a:rest) = (a, index) : actualBuses (index + 1) rest
actualBuses index (Nothing: rest) = actualBuses (index + 1) rest

part2 :: Int -> Int -> [(Int, Int)] -> Int
part2 time _ [] = time
part2 time qout ((bus, wait):rest) | (time + wait) `mod` bus == 0 = part2 time (qout * bus) rest 
                                   | otherwise                  = part2 (time + qout) qout ((bus, wait): rest)

exampleInput :: [Maybe Int]
exampleInput = map toBus . words . map replace $ "17,x,13,19"

main :: IO ()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let time = read . head $ ls :: Int
    let busses = map toBus . words . map replace $ ls !! 1
    let waitingTimes = sort . map (\bus -> (waitingTime time <$> bus, bus)) $ busses
    print waitingTimes
    let actuals = actualBuses 0 busses
    print (part2 0 1 actuals)
    return ()