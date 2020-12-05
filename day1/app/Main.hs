module Main where
import qualified Data.HashSet as S
import Data.Hashable
import Data.HashSet (HashSet)

find2020Tups :: Integral n => [n] -> [(n, n, n)]
find2020Tups ns = [(x,y, x*y) | 
                        x <- ns, 
                        y <- ns, 
                        x < y, 
                        x + y == 2020]

find2020Trips :: Integral n => [n] -> [(n, n, n, n)]
find2020Trips ns = [(x,y,z, x*y*z) | 
                        x <- ns, 
                        y <- ns, 
                        x < y, 
                        z <- ns, 
                        y < z, 
                        x + y + z == 2020]

findTups :: (Integral n, Hashable n) => [n] -> HashSet n -> HashSet (n,n,n)
findTups []     _   = S.empty 
findTups (x:xs) set = if diff `S.member` set then S.singleton (x, diff, x * diff) `S.union` rest else rest
    where diff = 2020 - x
          rest = findTups xs set

main :: IO ()
main = do
    f <- readFile "input.txt"
    let nums = (read <$> lines f) :: [Integer]
    let set = S.fromList nums
    print(find2020Tups nums)
    print(find2020Trips nums)


