module Main where
import Data.Bits
import Data.List (sort)
import qualified Data.Set as S

replace :: String -> Int
replace []       = zeroBits
replace ('F':xs) = replace xs
replace ('B':xs) = bit (length xs) .|. replace xs
replace ('L':xs) = replace xs
replace ('R':xs) = bit (length xs) .|. replace xs

possibleSeats :: S.Set Int
possibleSeats = S.fromList [23..828]

main :: IO ()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let seats = S.fromList $ map replace ls
    print $ S.findMax seats
    print $ S.difference possibleSeats seats
    
    return ()