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
          | Id String 
          | List [Expr]
          | Quot Expr
          | App Expr Expr
          | Abs String Expr
          | Let String Expr Expr
          deriving (Eq, Show)

parseString :: String -> Maybe Expr
parseString s = case runParser toplevel s of
  [] -> Nothing
  ss -> Just $ fst . head $ ss

parseNum :: String -> Maybe Expr
parseNum s = case runParser number s of
  [] -> Nothing
  n  -> Just $ Number $ fst . head $ n

toplevel :: Parser Expr
toplevel = app +++ list +++ quote

quote :: Parser Expr
quote = do
  string "'"
  s <- toplevel
  return $ Quot s

expr :: Parser Expr
expr = atom +++ quote +++ letExpr +++ app +++ list

app :: Parser Expr
app = do
  char '('
  s <- form
  char ')'
  return $ foldl1 App s

list :: Parser Expr
list = do
  string "(\\"
  s <- form
  char ')'
  return $ List s

form :: Parser [Expr]
form = do
  many whitespace
  s <- sepBy (some whitespace) expr
  many whitespace
  return s

atom :: Parser Expr
atom = numLit <|> boolLit <|> name

numLit :: Parser Expr
numLit = do
  n <- some digit
  return . Number . read $ n

boolLit :: Parser Expr
boolLit = true <|> false

false :: Parser Expr
false = do
  string "false"
  return $ Boolean False

true :: Parser Expr
true = do
  string "true"
  return $ Boolean True

name :: Parser Expr
name = Id <$> some (letter +++ digit +++ special)

letExpr :: Parser Expr
letExpr = do
  string "let"
  some whitespace
  lists <- sepBy (many whitespace) expr
  case lists of
    [Id i, e, inExpr] -> return $ Let i e inExpr
    _              -> failure
