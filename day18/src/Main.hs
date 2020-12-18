module Main where
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr (Operator (InfixL),
                                                 makeExprParser)
import           Data.Void                      (Void)
import           Text.Megaparsec                (Parsec, errorBundlePretty,
                                                 parse, some, (<|>))
import           Text.Megaparsec.Char           (space)
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

data Expr = Const Int
          | (:+:) Expr Expr
          | (:*:) Expr Expr
  deriving Show

exprP :: Parser Expr
exprP = makeExprParser termP operators2

-- operators for part 1
operators1 :: [[Operator Parser Expr]]
operators1 = [[binary "+" (:+:), binary "*" (:*:)]]

operators2 :: [[Operator Parser Expr]]
operators2 = [[binary "+" (:+:)], [binary "*" (:*:)]]

termP :: Parser Expr
termP = Const <$> L.lexeme space L.decimal <|> do
  L.symbol space "("
  expr <- exprP
  L.symbol space ")"
  return expr

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ L.symbol space name)

eval :: Expr -> Int
eval (Const n)     = n
eval ((:+:) e1 e2) = eval e1 + eval e2
eval ((:*:) e1 e2) = eval e1 * eval e2

allP :: Parser [Expr]
allP = some exprP

main :: IO ()
main = do
  file <- readFile "input.txt"
  case parse allP "input.txt" file of
    Left error  -> putStr (errorBundlePretty error)
    Right exprs -> print . sum $ eval <$> exprs

