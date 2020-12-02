{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

data Policy = Policy {lo :: Int,  hi :: Int, c :: Text}
    deriving (Eq, Show)


parseLine :: Text -> (Policy, Text)
parseLine line = (Policy lower upper char, word)
    where split = T.splitOn ": " line
          policySplit = T.splitOn " " (head split)
          lower = read . T.unpack . head . T.splitOn "-" . head $ policySplit
          upper = read . T.unpack . (!! 1) . T.splitOn "-" . head $ policySplit
          char = policySplit !! 1
          word = split !! 1

validateLine1 :: (Policy, Text) -> Bool
validateLine1 (policy, text) = count >= lo policy && count <= hi policy
    where count = T.count (c policy) text

validateLine2 :: (Policy, Text) -> Bool
validateLine2 (policy, text) = testLo xor testHi
    where char = head . T.unpack . c $ policy
          testLo = T.index text (lo policy - 1) == char
          testHi = T.index text (hi policy - 1) == char

xor :: Bool -> Bool -> Bool
xor = (/=)

main :: IO()
main = do
    file <- TIO.readFile "input.txt"
    let ls = T.lines file
    print "Solution part 1:"
    print $ length . filter validateLine1 $ parseLine <$> ls
    print "Solution part 2:"
    print $ length . filter validateLine2 $ parseLine <$> ls