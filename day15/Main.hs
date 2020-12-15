module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

input :: [Int]
input = [1,20,8,12,0,14]

exampleInput :: [Int]
exampleInput = [0,3,6]

data Spoken = Spoken {spoken :: IntMap Int, lastSpoken :: Int, stepsTaken :: Int, maxSteps :: Int }
    deriving (Show)

emptySpoken :: Spoken
emptySpoken = Spoken M.empty 0 0 30000000

fillStart :: [Int] -> Spoken -> Spoken
fillStart startnums (Spoken _ l st ms) = Spoken (M.fromList (zip (init startnums) [1..])) (last startnums) (length startnums) ms

run :: Spoken -> Int
run (Spoken sp l st ms) = if st >= ms then l else
    case M.lookup l sp of
        Just index -> run $ Spoken (M.insert l st sp) (st - index) (st + 1) ms
        Nothing -> run $ Spoken (M.insert l st sp) 0 (st + 1) ms


main :: IO ()
main = do
    let initSpoken = fillStart input emptySpoken
    print . run $ initSpoken 