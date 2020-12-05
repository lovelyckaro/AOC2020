module Main where

data Tile = Tree | Open
    deriving (Eq, Ord, Show)

type Forest = [[Tile]]

readTile :: Char -> Tile
readTile '.' = Open
readTile '#' = Tree
readTile x   = error ("Could not read char " ++ [x])

--           Down, Right
travelVec :: (Int, Int)
travelVec = (1,3)

travelVecs :: [(Int, Int)]
travelVecs = [
    (1,1),
    (1,3),
    (1,5),
    (1,7),
    (2,1)]

walk :: Forest -> (Int, Int) -> Int
walk f (d,r) = length $ filter (==Tree) [(f !! (k * d)) !! (k * r `mod` lineLen ) | k <- [0..end-1]]
    where end = length f `div` d
          lineLen = length . head $ f
          

main :: IO ()
main = do
    file <- readFile "input.txt"
    let forest = map readTile <$> lines file
    print (walk forest travelVec)
    print (walk forest <$> travelVecs)