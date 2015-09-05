{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language(
  Expr(..),
  parseString,
  parseNum,
) where

import Parser
import Control.Applicative

data Expr = Number Int
          | Boolean Bool
          | Chars String
          | Id String 
          | List [Expr]
          | Quot Expr
          | App Expr Expr
          | Abs String Expr
          | Let String Expr Expr
          | Def String Expr
          deriving (Eq)

instance Show Expr where
  show (Number n)    = show n
  show (Boolean b)   = show b
  show (Chars cs)    = show cs
  show (List exs)    = show exs
  show (Quot e)      = "'" ++ show e
  show (App e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Abs s e)     = "(\\" ++ s ++ ". " ++ show e ++ ")"
  show (Let s e1 e2) = "Let " ++ s ++ " " ++ show e1 ++ " " ++ show e2
  show (Def s _)     = s
  show (Id s)        = s

parseString :: String -> Maybe Expr
parseString s = case runParser expr s of
  [] -> Nothing
  ss -> Just $ fst . head $ ss

parseNum :: String -> Maybe Expr
parseNum s = case runParser number s of
  [] -> Nothing
  n  -> Just $ Number $ fst . head $ n

quote :: Parser Expr
quote = do
  string "'"
  s <- expr
  return $ Quot s

expr :: Parser Expr
expr = atom +++ quote +++ lambda +++ def +++ letExpr +++ app +++ list

app :: Parser Expr
app = do
  char '('
  s <- form
  char ')'
  return $ foldl1 App s

list :: Parser Expr
list = do
  char '['
  s <- form
  char ']'
  return $ List s

form :: Parser [Expr]
form = do
  many whitespace
  s <- sepBy (some whitespace) expr
  many whitespace
  return s

atom :: Parser Expr
atom = numLit <|> boolLit <|> name <|> stringLit

numLit :: Parser Expr
numLit = do
  n <- some digit
  return . Number . read $ n

stringLit :: Parser Expr
stringLit = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return $ Chars s

boolLit :: Parser Expr
boolLit = true <|> false

false :: Parser Expr
false = do
  string "False"
  return $ Boolean False

true :: Parser Expr
true = do
  string "True"
  return $ Boolean True

name :: Parser Expr
name = Id <$> some (letter +++ digit +++ special)

lambda :: Parser Expr
lambda = do
  string "(\\"
  many whitespace
  is   <-  sepBy (some whitespace) name
  many whitespace
  char '.'
  many whitespace
  e    <- expr
  many whitespace
  char ')'
  return $ foldr (\(Id i) -> Abs i) e is

letExpr :: Parser Expr
letExpr = do
  string "(let"
  some whitespace
  Id i <- name
  some whitespace
  e    <- expr 
  some whitespace
  e'   <- expr
  many whitespace
  char ')'
  return $ Let i e e'

def :: Parser Expr
def = do
  string "(def"
  some whitespace
  Id i <- name
  some whitespace
  e    <- expr 
  many whitespace
  char ')'
  return $ Def i e
