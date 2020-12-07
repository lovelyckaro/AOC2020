{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Data.Map (Map, (!?))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe (maybeToList)

type Inner = Text
type Outer = Text
type Amount = Int

parseLine :: Text -> (Outer, [(Inner, Amount)])
parseLine txt = (outer, inners)
    where (outer, tmp) = T.breakOn " : " txt
          inners :: [(Inner, Amount)]
          inners = parseInner . T.splitOn ", " . T.drop 3 $ tmp
          parseInner :: [Text] -> [(Inner, Amount)]
          parseInner [] = []
          parseInner ("no other":xs) = parseInner xs
          parseInner (x:xs)          = (T.drop 2 x, read . T.unpack . T.take 1 $ x) : parseInner xs

-- Point of the exercise is to go from outer -> list of inners
-- to                                  inner -> list of possible outers

--      outer, inners            inner, outer
rev :: (Outer, [(Inner, Amount)]) -> [(Inner, [Outer])]
rev (_ , []) = []
rev (outer, (inner,_):xs) = (inner, [outer]) : rev (outer, xs)

--            input -> Map inner, possible outers
fullyParse1 :: Text -> Map Inner [Outer]
fullyParse1 t = foldr (M.unionWith (++)) M.empty maps
    where ls = T.lines t
          maps :: [Map Inner [Outer]]
          maps = M.fromList . rev . parseLine <$> ls

possibleContainers :: Map Inner [Outer] -> Inner -> Set Outer
possibleContainers tb inner = 
    case tb !? inner of
        Just outers -> foldr (S.union) (S.fromList outers) (possibleContainers tb <$> outers)
        Nothing -> S.empty
    
requiredBags :: Map Outer [(Inner, Amount)] -> Inner -> Amount
requiredBags tb outer = 
    case tb !? outer of
        Just [] -> 1
        --         outer + sum of requiredBags of all the inners
        Just bags -> 1 + sum (map (\(i, a) -> a * requiredBags tb i) bags)
        Nothing -> 1

main :: IO ()
main = do
    file <- TIO.readFile "input.txt"
    let tb1 = fullyParse1 file
    let tb2 = M.fromList $ parseLine <$> T.lines file
    print (S.size $ possibleContainers tb1 "shiny gold")
    print (requiredBags tb2 "shiny gold" - 1)