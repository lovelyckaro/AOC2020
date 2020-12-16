module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad(void)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

{-
Data types
-}
data Rule = Rule {field :: String, ranges :: [(Int, Int)]}
  deriving (Show, Eq, Ord)

type Ticket = [Int]

{-
Parser stuff
-}

type Parser = Parsec Void String

pRange :: Parser (Int, Int)
pRange = do
  lo <- some digitChar
  void (char '-')
  hi <- some digitChar
  void (string " or ") <|> void eol
  return (read lo, read hi)

pRule :: Parser Rule
pRule = do
  name <- some (letterChar <|> char ' ')
  void (string ": ")
  rs <- some pRange
  return (Rule name rs)

pRules :: Parser [Rule]
pRules = some pRule

pTicket :: Parser Ticket
pTicket = do
  nums <- some $ do
    num <- some digitChar
    void (optional (char ','))
    return num
  void eol <|> eof
  return (map read nums)

pAll :: Parser ([Rule], Ticket, [Ticket])
pAll = do
  rules <- pRules
  void eol
  void $ string "your ticket:\n"
  myticket <- pTicket
  void eol
  void $ string "nearby tickets:\n"
  nearby <- some pTicket
  return (rules, myticket, nearby)

{-
Applying the rules to the fields
-}

-- |Applies rule to each field in ticket
applyRule :: Rule -> Ticket -> [Bool]
applyRule (Rule _ []) ticket = repeat False
applyRule (Rule name ((lo, hi): rest)) ticket = 
  zipWith (||) 
    ((\n -> n >= lo && n <= hi) <$> ticket)
    (applyRule (Rule name rest) ticket)

-- |Applies all rules to ticket, if a field passes any rule it passes the entire thing
testTicket :: [Rule] -> Ticket -> [Bool]
testTicket rules ticket = helper rules ticket
  where 
    helper :: [Rule] -> Ticket -> [Bool]
    helper [] ticket = repeat False
    helper (rule:rest) ticket = zipWith (||) (applyRule rule ticket) (helper rest ticket)

-- |Applies all rules to ticket and sets valid fields to 0
invalidFields :: [Rule] -> Ticket -> Ticket
invalidFields rules ticket = zipWith (\b n -> if not b then n else 0) valids ticket
  where valids = testTicket rules ticket

-- |Applies a rule to all tickets and returns a list of bools
-- where if all fields at that index passed the rule, it is true,
-- otherwise it is false
candidates :: Rule -> [Ticket] -> [Bool]
candidates rule = foldr (zipWith (&&) . applyRule rule) (repeat True)

-- |All indexes matching list !! index == True
indexesOfTrue :: [Bool] -> [Int]
indexesOfTrue = helper 0
  where helper n [] = []
        helper n (True:rest) = n : helper (n+1) rest
        helper n (False: rest) = helper (n+1) rest

-- |Takes a Map from Rule to possible indexes, sees if there are any determined
-- indexes and removes them from all others
reducePossibles :: Map Rule (Set Int) -> Map Rule (Set Int)
reducePossibles m = singles `M.union` M.map ( S.\\ indexesToRemove) others
  where (singles, others) = M.partition (\s -> S.size s == 1) m
        indexesToRemove = foldr S.union S.empty (M.elems singles)

-- | Reduce until you cant reduce no more        
fullyReduce :: Map Rule (Set Int) -> Map Rule (Set Int)
fullyReduce m | reducePossibles m == m = m
              | otherwise              = fullyReduce (reducePossibles m)

main :: IO ()
main = do
  file <- readFile "input.txt"
  case parse pAll "input.txt" file of
    Left error -> putStr (errorBundlePretty error)
    Right (rules, myticket, nearby) -> do
      -- filtered removes nearby with fields that do not pass any rule
      let filtered = filter (and . testTicket rules) nearby
      -- Make map from rules to possible indices 
      let m = M.fromList (zip rules (S.fromAscList <$> (indexesOfTrue . flip candidates filtered <$> rules)))
      -- ruleToIndex is that map, fully reduced
      let ruleToIndex = M.map (head . S.elems) (fullyReduce m)
      -- get the indices of the first 6 rules in the input
      let indices = [ruleToIndex M.! rule | rule <- take 6 rules]
      print(product [myticket !! index | index <- indices])
