{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language(
  Expr(..),
  parseString,
  parseNum,
  match,
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
          | Abs Expr Expr
          | PAbs [Expr] -- TODO: function def with multiple patterns
          | Let String Expr Expr
          | Def String Expr
          | Data String [String] [(String, [Expr])]
          deriving (Eq)

instance Show Expr where
  show (Number n)    = show n
  show (Boolean b)   = show b
  show (Chars cs)    = show cs
  show (List exs)    = show exs
  show (Quot e)      = "'" ++ show e
  show (App e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Abs s e)     = "(\\" ++ show s ++ ". " ++ show e ++ ")"
  show (Let s e1 e2) = "Let " ++ s ++ " " ++ show e1 ++ " " ++ show e2
  show (Def s _)     = s
  show (Id s)        = s
  show (Data n _ _)  = n

type Pattern = Expr
match :: Pattern -> Expr -> Maybe [(String, Expr)]
match (App (Id cons1) (Id r1)) (App (Id cons2) r2)
  | cons1 == cons2 = Just [(r1, r2)]
  | otherwise      = Nothing
match (App l1 (Id r1)) (App l2 r2)
  = fmap ((r1, r2) :) (match l1 l2)
match (Id p) x
  = Just [(p, x)]
match a b
  | a == b    = Just []
  | otherwise = Nothing

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

-- TODO: def and dataDef should only be allowed at the top level
expr :: Parser Expr
expr = atom +++ quote +++ lambda +++ def +++ dataDef +++ letExpr +++ app +++ list

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
name = Id <$> nameS

nameS :: Parser String
nameS = some (letter +++ digit +++ special)

lambda :: Parser Expr
lambda = do
  string "(\\"
  many whitespace
  is   <-  sepBy (some whitespace) (expr)
  many whitespace
  char '.'
  many whitespace
  e    <- expr
  many whitespace
  char ')'
  return $ foldr Abs e is

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

dataDef :: Parser Expr
dataDef = do
  string "(data"
  some whitespace
  char '('
  many whitespace
  Id i  <- name
  tvars <- (some whitespace >> sepBy (some whitespace) nameS) <|> return []
  many whitespace
  char ')'
  many whitespace
  cons  <- sepBy (many whitespace) constructor
  many whitespace
  char ')'
  return $ Data i tvars cons

constructor :: Parser (String, [Expr])
constructor = do
  char '['
  many whitespace
  Id i  <- name
  tvars <- (some whitespace >> sepBy (some whitespace) name) <|> return []
  many whitespace
  char ']'
  return (i, tvars)
