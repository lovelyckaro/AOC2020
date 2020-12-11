module Main where

import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

data Tile = Chair | Taken | Floor
    deriving (Eq, Show)

parseChar :: Char -> Tile
parseChar '.' = Floor
parseChar '#' = Taken
parseChar 'L' = Chair
parseChar _   = error "Parse failed"


type State = Vector (Vector Tile)

tickTile1 :: State -> (Int, Int) -> ((Int, Int), Tile)
tickTile1 state (x,y) = case tile of
                            Chair -> if occupied == 0 then ((x,y),Taken) else ((x,y), Chair)
                            Taken -> if occupied >= 4 then ((x,y), Chair) else ((x,y), Taken)
                            Floor -> ((x,y), Floor)
    where indices = [(x+dx, y+dy) |
                        dx <- [-1,0,1],
                        dy <- [-1,0,1],
                        not (dx == 0 && dy == 0),
                        x+dx >= 0,
                        x+dx < V.length(V.head state),
                        y+dy >= 0,
                        y+dy < V.length state]
          surrounding = get state <$> indices
          occupied :: Int
          occupied = length $ filter (==Taken) surrounding
          tile = get state (x,y)

tickTile2 :: State -> (Int, Int) -> ((Int, Int), Tile)
tickTile2 state (x,y) = case tile of
                            Chair -> if occupied == 0 then ((x,y),Taken) else ((x,y), Chair)
                            Taken -> if occupied >= 5 then ((x,y), Chair) else ((x,y), Taken)
                            Floor -> ((x,y), Floor)
    where vectors = [(dx, dy) |
                        dx <- [-1,0,1],
                        dy <- [-1,0,1],
                        not (dx == 0 && dy == 0),
                        x+dx >= 0,
                        x+dx < V.length(V.head state),
                        y+dy >= 0,
                        y+dy < V.length state]
          surrounding = look state (x,y) <$> vectors
          occupied :: Int
          occupied = length $ filter (==Taken) surrounding
          tile = get state (x,y)

firstChair :: [Tile] -> Tile
firstChair [] = Floor
firstChair (Taken:_) = Taken
firstChair (Chair:_) = Chair
firstChair (Floor:xs) = firstChair xs


look :: State -> (Int, Int) -> (Int, Int) -> Tile
look state point vector = firstChair (get state <$> indices)
    where xs = [x + a*dx | a <- [1..width], x + a*dx >= 0, x + a*dx < width]
          ys = [y + a*dy | a <- [1..height], y + a*dy >= 0, y + a*dy < height]
          indices = zip xs ys
          (x,y) = point
          (dx,dy) = vector
          width = V.length(V.head state)
          height = V.length state

tick :: State -> State
tick state = setTiles state (tickTile2 state <$> indices)
    where indices = [(x,y) | x <- [0..length (V.head state) - 1], y <- [0.. length state - 1]]

setTile :: State -> ((Int, Int), Tile) -> State
setTile state ((x,y),newTile) = state // [(y, state ! y // [(x,newTile)])]

setTiles :: State -> [((Int, Int), Tile)] -> State
setTiles = foldl setTile

get :: State -> (Int, Int) -> Tile
get state (x,y) = (state ! y) ! x

run :: State -> State
run state | tick state == state = state
          | otherwise = run (tick state)

getInput :: FilePath -> IO State
getInput fp = do
    file <- readFile fp
    return $ V.fromList (V.fromList . map parseChar <$> lines file)

countOccupied :: State -> Int
countOccupied state = sum $ V.length . V.filter (==Taken)  <$> state

toChar :: Tile -> Char
toChar Chair = 'L'
toChar Taken = '#'
toChar Floor = '.'

showState :: State -> String
showState state = unlines $ V.toList <$> V.toList ( V.map toChar <$> state)

printState :: State -> IO ()
printState = putStr . showState

main :: IO ()
main = do
    initState <- getInput "input.txt"
    let state = run initState
    print (countOccupied state)
    return ()
