module Main where

import Data.Map (Map, (\\))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    errorBundlePretty,
    parse,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (eol, letterChar, space, string)

type Parser = Parsec Void String

newtype Allergen = Allergen {getAllergen :: String}
  deriving (Show, Eq, Ord)

newtype Ingredient = Ingredient {getIngredient :: String}
  deriving (Show, Eq, Ord)

data Line = Line {ingredients :: [Ingredient], allergens :: [Allergen]}
  deriving (Show)

toMap :: Line -> Map Allergen (Set Ingredient)
toMap (Line ins alls) = M.fromList [(allergen, S.fromList ins) | allergen <- alls]

toMaps :: [Line] -> [Map Allergen (Set Ingredient)]
toMaps = map toMap

pLines :: Parser [Line]
pLines = some pLine

pLine :: Parser Line
pLine = do
  ingredients <- pIngredients
  allergens <- pAllergens
  eol
  return (Line ingredients allergens)

pIngredients :: Parser [Ingredient]
pIngredients = some pIngredient

pIngredient :: Parser Ingredient
pIngredient = do
  w <- some letterChar
  space
  return (Ingredient w)

pAllergens :: Parser [Allergen]
pAllergens = do
  string "(contains "
  allergens <- some $ do
    allergen <- some letterChar
    string ", " <|> string ")"
    return allergen
  return (Allergen <$> allergens)

reducePossibles :: [Map Allergen (Set Ingredient)] -> Map Allergen (Set Ingredient)
reducePossibles = foldr combine M.empty
  where
    combine = M.unionWith S.intersection

allIngredients :: [Line] -> Map Ingredient Int
allIngredients xs = foldr (\i m -> M.insertWith (+) i 1 m) M.empty (concatMap ingredients xs)

removePossibles :: Map Ingredient Int -> Map Allergen (Set Ingredient) -> Map Ingredient Int
removePossibles all possibles = all \\ M.fromSet undefined removables
  where
    removables :: Set Ingredient
    removables = foldr S.union S.empty (M.elems possibles)

decideAllergens :: Map Allergen (Set Ingredient) -> Map Allergen Ingredient
decideAllergens m = helper m M.empty
  where
    helper :: Map Allergen (Set Ingredient) -> Map Allergen Ingredient -> Map Allergen Ingredient
    helper uncertain known
      | M.null uncertain = known
      | otherwise = helper (removeSingletons uncertain) (addSingletons uncertain known)
    removeSingletons :: Map Allergen (Set Ingredient) -> Map Allergen (Set Ingredient)
    removeSingletons xs = M.map (removeThese (M.elems (singletons xs))) (xs \\ singletons xs)
    addSingletons :: Map Allergen (Set Ingredient) -> Map Allergen Ingredient -> Map Allergen Ingredient
    addSingletons xs known = M.union known (singletons xs)
    singletons :: Map Allergen (Set Ingredient) -> Map Allergen Ingredient
    singletons = M.map S.findMin . M.filter (\s -> S.size s == 1)
    removeThese :: [Ingredient] -> Set Ingredient -> Set Ingredient
    removeThese xs ys = ys S.\\ S.fromList xs

part1 :: [Line] -> Int
part1 xs = sum (removePossibles allIns dangerous)
  where
    allIns = allIngredients xs
    dangerous = reducePossibles . toMaps $ xs

part2 :: [Line] -> String
part2 xs = foldr1 (\s1 s2 -> s1 <> "," <> s2) list
  where
    dangerous = reducePossibles . toMaps $ xs
    mapping = decideAllergens dangerous
    list = map (getIngredient . snd) . M.toAscList $ mapping

main :: IO ()
main = do
  inp <- readFile "input"
  case parse pLines "input" inp of
    Right xs -> do
      print (part1 xs)
      print (part2 xs)
    Left error -> putStrLn (errorBundlePretty error)