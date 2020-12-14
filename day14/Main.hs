module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (isPrefixOf)
import Data.Bits

data State1 = State1 {ormask :: Int, andmask :: Int, memory1 :: IntMap Int}
    deriving (Show, Eq, Ord)

data State2 = State2 {masks :: [(Int, Int)], memory2 :: IntMap Int}
    deriving (Show, Eq, Ord)

getMasks1 :: String -> (Int, Int)
getMasks1 str = (omask, amask)
    where omask = helper 35 $ map (\c -> if c == 'X' then '0' else c) str
          amask = helper 35 $ map (\c -> if c == 'X' then '1' else c) str
          helper _ [] = zeroBits
          helper index ('1':xs) = helper (index - 1) xs `setBit` index
          helper index (_:xs) = helper (index - 1) xs

getMasks2 :: String -> [(Int, Int)]
getMasks2 str = zip (omasks 35 str) (amasks 35 str)
    where
        omasks _ [] = [zeroBits]
        omasks index ('1':xs) = map (`setBit` index) $ omasks (index - 1) xs
        omasks index ('0':xs) = map (`clearBit` index) $ omasks (index - 1) xs
        omasks index ('X':xs) = omasks index ('1':xs) ++ omasks index ('0':xs)
        amasks _ [] = [zeroBits]
        amasks index ('1':xs) = map (`setBit` index) $ amasks (index - 1) xs
        amasks index ('0':xs) = map (`setBit` index) $ amasks (index - 1) xs
        amasks index ('X':xs) = amasks index ('1':xs) ++ map (`clearBit` index) ( amasks (index - 1) xs)


getAssign :: String -> (Int, Int)
getAssign str = (index, value)
    where index = read . drop 1 . takeWhile (/= ']') $ str
          value = read . drop 1 . dropWhile (/= '=') $ str

updateState1 :: State1 -> String -> State1
updateState1 (State1 omask amask mem) str | "mask" `isPrefixOf` str = 
    let (omask', amask') = getMasks1 (drop 7 str) in State1 omask' amask' mem
                                        | otherwise               = 
    let (index, value) = getAssign (drop 3 str) in State1 omask amask (M.insert index (amask .&. (omask .|. value)) mem)

updateState2 :: State2 -> String -> State2
updateState2 (State2 masks mem) str | "mask" `isPrefixOf` str = State2 (getMasks2 (drop 7 str)) mem
                                    | otherwise =
    let (index, value) = getAssign (drop 3 str) in State2 masks (M.fromList [(amask .&. (omask .|. index), value) | (omask, amask) <- masks] `M.union` mem)

initState1 :: State1
initState1 = State1 0 0 M.empty

initState2 :: State2
initState2 = State2 [] M.empty

main :: IO ()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    print (M.foldr (+) 0 . memory1 $ foldl updateState1 initState1 ls)
    print (M.foldr (+) 0 . memory2 $ foldl updateState2 initState2 ls)
    return ()