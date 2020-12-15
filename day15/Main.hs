{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (elemIndex)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M

input :: [Int]
input = [1,20,8,12,0,14]

exampleInput :: [Int]
exampleInput = [0,3,6]

data Spoken = Spoken {spoken :: IntMap Int, lastSpoken :: Int, stepsTaken :: Int, maxSteps :: Int }
    deriving (Show)

emptySpoken :: Spoken
emptySpoken = Spoken M.empty 0 0 3000000

fillStart :: [Int] -> Spoken -> Spoken
fillStart startnums (Spoken _ l st ms) = Spoken (M.fromList (zip (init startnums) [1..])) (last startnums) (length startnums) ms

step :: Spoken -> Spoken
step (Spoken sp l st ms) = case M.lookup l sp of
        Just index -> Spoken (M.insert l st sp) (st - index) (st + 1) ms
        Nothing -> Spoken (M.insert l st sp) 0 (st + 1) ms
    
run :: [Int] -> [Int]
run inits = lastSpoken <$> iterate step (fillStart inits emptySpoken )


main :: IO ()
main = do
    print $ (run input) !! (30000000 -6)