{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad        (void)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Policy = Policy {lo :: Int,  hi :: Int, c :: Text}
    deriving (Eq, Show)

type Parser = Parsec Void Text

lineP :: Parser (Policy, Text)
lineP = do
    lo <- some digitChar -- parse lo
    void (char '-')      -- expect '-'
    hi <- some digitChar -- parse hi
    space                -- expect  ' '
    c <- letterChar      -- parse c
    void (string ": ")   -- expect ': '
    password <- some alphaNumChar -- parse password
    void eol <|> eof                  -- expect end of line or end of file
    return (Policy (read lo) (read hi) (T.singleton c), T.pack password)

fullP :: Parser [(Policy, Text)]
fullP = some lineP

validateLine1 :: (Policy, Text) -> Bool
validateLine1 (policy, text) = count >= lo policy && count <= hi policy
    where count = T.count (c policy) text

validateLine2 :: (Policy, Text) -> Bool
validateLine2 (policy, text) = testLo `xor` testHi
    where char = head . T.unpack . c $ policy
          testLo = T.index text (lo policy - 1) == char
          testHi = T.index text (hi policy - 1) == char

xor :: Bool -> Bool -> Bool
xor = (/=)

main :: IO()
main = do
    file <- TIO.readFile "input.txt"
    case runParser fullP "input.txt" file of
        Left error -> putStr (errorBundlePretty error)
        Right ls -> do
            print "Solution part 1:"
            print (length . filter validateLine1 $ ls)
            print "Solution part 2:"
            print (length . filter validateLine2 $ ls)
