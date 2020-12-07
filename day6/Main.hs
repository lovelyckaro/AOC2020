{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Set (Set)

main :: IO ()
main = do
    file <- TIO.readFile "input.txt"
    let strings = T.splitOn "\n\n" file
    let groups1 = S.fromList . T.unpack . T.filter (/= '\n') <$> strings
    let sizes1 = S.size <$> groups1
    let sets2 = map S.fromList . lines <$> map T.unpack strings
    let groups2 = foldr S.intersection (S.fromList ['a'..'z']) <$> sets2
    print(sum $ map S.size groups2)
    return()


