{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Map (Map, (!?))
import Data.Text (Text)

validateByr :: Text -> Bool
validateByr t = T.length t == 4 && year >= 1920 && year <= 2002
    where year :: Int
          year = read . T.unpack $ t

validateIyr :: Text -> Bool
validateIyr t = T.length t == 4 && year >= 2010 && year <= 2020
    where year :: Int
          year = read . T.unpack $ t

validateEyr :: Text -> Bool
validateEyr t = T.length t == 4 && year >= 2020 && year <= 2030
    where year :: Int
          year = read . T.unpack $ t

validateHgt :: Text -> Bool
validateHgt t = 
    case suffix of
        "cm" -> height >= 150 && height <= 193
        "in" -> height >= 59 && height <= 76
        _ -> False
    where suffix = T.takeEnd 2 t
          height :: Int
          height = read . T.unpack . T.dropEnd 2 $ t

validateHcl :: Text -> Bool
validateHcl t = "#" `T.isPrefixOf` t && testRest
    where validChars = ['0'..'9'] ++ ['a'..'f']
          chartest :: Char -> Bool
          chartest c = c `elem` validChars
          testRest = T.length (T.filter chartest (T.drop 1 t)) == 6

validateEcl :: Text -> Bool
validateEcl t = or $ (t ==) <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePid :: Text -> Bool
validatePid t = T.length t == 9 && T.length (T.filter validChars t) == 9
    where validChars :: Char -> Bool
          validChars c = c `elem` ['0'..'9']

toKeyValue :: Text -> (Text, Text)
toKeyValue t = (T.take 3 t, T.drop 4 t)

validateTable1 :: Map Text Text -> Bool
validateTable1 m = and $ flip M.member m <$> ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

validateTable2 :: Map Text Text -> Bool
validateTable2 m = mbAnd $ zipWith fmap funs keys
                        where funs = [validateEcl, validatePid, validateEyr, validateHcl, validateByr, validateIyr, validateHgt]
                              keys = (m !?) <$> ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"] 

mbAnd :: [Maybe Bool] -> Bool
mbAnd = all (== Just True)

main :: IO ()
main = do
    file <- TIO.readFile "input.txt"
    let passes = M.fromList . map toKeyValue . T.words <$> T.splitOn "\n\n" file
    print (length . filter validateTable1 $ passes )
    print (length . filter validateTable2 $ passes)
    return ()