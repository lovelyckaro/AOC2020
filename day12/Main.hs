module Main where

data Instruction = North Int
                 | South Int
                 | East Int
                 | West Int
                 | Left' Int
                 | Right' Int
                 | Forward Int
                 deriving (Show, Eq)

parseInstr :: String -> Instruction
parseInstr ('N':rest) = North (read rest)
parseInstr ('S':rest) = South (read rest)
parseInstr ('E':rest) = East (read rest)
parseInstr ('W':rest) = West (read rest)
parseInstr ('L':rest) = Left' (read rest)
parseInstr ('R':rest) = Right' (read rest)
parseInstr ('F':rest) = Forward (read rest)

-- 0 deg == East, 90 deg == North, 180 deg == west, 270 deg == south
-- boat starts at (x,y) = (0,0)
-- Going north adds to y
-- Going south subtracts from y
-- Going east adds to x
-- Going west subtracts from x

data Boat = Boat {boatX :: Int, boatY :: Int, heading :: Int, waypointX :: Int, waypointY :: Int}
  deriving (Eq, Ord, Show)

initBoat :: Boat
initBoat = Boat 0 0 0 10 1

step1 :: Boat -> Instruction -> Boat
step1 (Boat x y degs wx wy) (North amount)   = Boat x (y+amount) degs wx wy
step1 (Boat x y degs wx wy) (South amount)   = Boat x (y-amount) degs wx wy
step1 (Boat x y degs wx wy) (West amount)    = Boat (x-amount) y degs wx wy
step1 (Boat x y degs wx wy) (East amount)    = Boat (x+amount) y degs wx wy
step1 (Boat x y degs wx wy) (Left' amount)   = Boat x y ((degs + amount) `mod` 360) wx wy
step1 (Boat x y degs wx wy) (Right' amount)  = Boat x y ((degs - amount) `mod` 360) wx wy
step1 boat (Forward amount) = 
  case heading boat of
    0 -> step1 boat (East amount)
    90 -> step1 boat (North amount)
    180 -> step1 boat (West amount)
    270 -> step1 boat (South amount)
    x -> error (show x)   

step2 :: Boat -> Instruction -> Boat
step2 (Boat x y degs wx wy) (North amount)   = Boat x y degs wx (wy + amount)
step2 (Boat x y degs wx wy) (South amount)   = Boat x y degs wx (wy - amount)
step2 (Boat x y degs wx wy) (West amount)    = Boat x y degs (wx - amount) wy
step2 (Boat x y degs wx wy) (East amount)    = Boat x y degs (wx + amount) wy
step2 (Boat x y degs wx wy) (Left' amount)   = Boat x y degs wx' wy'
    where angle = amount `mod` 360
          wx' = badCos angle * wx - badSin angle * wy
          wy' = badSin angle * wx + badCos angle * wy
step2 boat (Right' amount)  = step2 boat (Left' (-amount))
step2 (Boat x y degs wx wy) (Forward amount) = Boat (x + amount * wx) (y + amount * wy) degs wx wy

badSin :: Int -> Int
badSin 0 = 0
badSin 90 = 1
badSin 180 = 0
badSin 270 = -1

badCos :: Int -> Int
badCos 0 = 1
badCos 90 = 0
badCos 180 = -1
badCos 270 = 0

main :: IO ()
main = do
  file <- readFile "input.txt"
  let instrs = parseInstr <$> lines file
  print (foldl step1 initBoat instrs)
  print (foldl step2 initBoat instrs)
  return ()