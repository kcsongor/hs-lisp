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
          | Cond Expr Expr Expr
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
toplevel = list +++ quote

quote :: Parser Expr
quote = do
  char '\''
  s <- list
  return $ Quot s

expr :: Parser Expr
expr = quote +++ ifExpr +++ letExpr +++ atom +++ list

list :: Parser Expr
list = do
  char '('
  many whitespace
  s <- sepBy (some whitespace) expr
  many whitespace
  char ')'
  return $ List s

atom :: Parser Expr
atom = Id <$> some (letter +++ digit +++ special)

ifExpr :: Parser Expr
ifExpr = do
  string "if"
  some whitespace
  lists <- sepBy (many whitespace) expr
  case lists of
    [c, t, f] -> return $ Cond c t f
    _         -> failure

letExpr :: Parser Expr
letExpr = do
  string "let"
  some whitespace
  lists <- sepBy (many whitespace) expr
  case lists of
    [Id i, e, inExpr] -> return $ Let i e inExpr
    _              -> failure
