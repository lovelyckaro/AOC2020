module Main where

import Control.Monad (guard, void)
import Data.IntMap (IntMap, (!), (!?))
import qualified Data.IntMap as M
import Data.Maybe (fromJust, isJust)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    choice,
    errorBundlePretty,
    many,
    optional,
    parse,
    parseMaybe,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    eol,
    hspace,
    letterChar,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug ()

type Parser = Parsec Void String

data Rule = Constant String | And [Int] | Or [Rule]
  deriving (Show)

type Ruleset = IntMap Rule

-- Parser for all rules
rulesP :: Parser Ruleset
rulesP = M.fromList <$> some ruleP

{-
Parser for a rule
expect the rule number first
and then either a list of alternative lists of references to other rules
or an endpoint rule "a"/"b"
-}
ruleP :: Parser (Int, Rule)
ruleP = do
  n <- L.decimal
  void (string ": ")
  r <- (Or <$> listsP) <|> (Constant <$> strP)
  void eol <|> eof
  return (n, r)

-- parser for an endpoint rule
strP :: Parser String
strP = do
  void (char '\"')
  c <- letterChar
  void (char '\"')
  return [c]

-- parser for a rule which references other rules
lstP :: Parser Rule
lstP = And <$> some (L.lexeme hspace L.decimal)

-- parser for all alternatives in a rule
listsP :: Parser [Rule]
listsP = some $ do
  ands <- lstP
  void (optional $ L.symbol hspace "|")
  return ands

-- Parser for a message
messageP :: Parser String
messageP = do
  str <- some letterChar
  void eol <|> eof
  return str

-- Parser for all the messages
messagesP :: Parser [String]
messagesP = some messageP

inputP :: Parser (Ruleset, [String])
inputP = do
  rules <- rulesP
  void eol
  messages <- messagesP
  return (rules, messages)

toParser1 :: Ruleset -> Parser String
toParser1 rs = ruleToParser rs (rs ! 0)

{-
Part 2 is easier than one might expect at first glance.
All rule 0 does is parse one rule 8 and then one rule 11 (our loops)
Rule 8 parses x number of rule 42s
Rule 11 parses y number of rule 42s and then y number of rule 31s

Nowhere else in the rules are rule 0, 8 or 11 featured

This means that we can combine these three rules into one new entrypoint
where we ask for at least 2 rule 42s and at least 1 31,
where the number of thirtyones is strictly less than the number of 42s
-}
toParser2 :: Ruleset -> Parser String
toParser2 rs = do
  fortytwos <- many (ruleToParser rs (rs ! 42))
  thirtyones <- many (ruleToParser rs (rs ! 31))
  guard (length thirtyones < length fortytwos)
  guard (length fortytwos >= 2)
  guard (length thirtyones >= 1)
  return (concat fortytwos ++ concat thirtyones)

ruleToParser :: Ruleset -> Rule -> Parser String
ruleToParser rs (Constant str) = string str
ruleToParser rs (Or rules) = choice [try (ruleToParser rs rule) | rule <- rules]
ruleToParser rs (And nums) = concat <$> sequence [ruleToParser rs (rs ! n) | n <- nums]

example :: Ruleset
example = rules
  where
    Right rules = parse rulesP "" "0: 1 2\n1: \"a\"\n2: 1 3 | 3 1\n3: \"b\""

main :: IO ()
main = do
  file <- readFile "input.txt"
  case parse inputP "input.txt" file of
    Left error -> putStr (errorBundlePretty error)
    Right (rules, messages) -> do
      let parser2 = toParser2 rules
      let parser1 = toParser1 rules
      print . length . filter isJust $ parseMaybe parser1 <$> messages
      print . length . filter isJust $ parseMaybe parser2 <$> messages