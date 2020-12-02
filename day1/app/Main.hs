module Main where

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

main :: IO ()
main = do
    f <- readFile "input.txt"
    let nums = (read <$> lines f) :: [Integer]
    print(find2020Tups nums)
    print(find2020Trips nums)


