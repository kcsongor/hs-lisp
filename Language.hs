{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GADTs #-}
module Language(
  Expr(..),
  parseExpr,
  parseCode,
  parseNum,
  match,
) where

import Parser
import Control.Applicative

type Pattern = Expr
data Expr = Number Int
          | Chars String
          | Id String 
          | Cons String 
          | List [Expr]
          | Quot Expr
          | App Expr Expr
          | Lam Expr Expr
          | PAbs [Expr] -- List of Lams
          | PApp [(Expr, Expr)]
          | Let Expr Expr Expr
          | Def String Expr
          | Data String [String] [(String, [Expr])]
          deriving (Eq)

instance Show Expr where
  show (Number n)    = show n
  show (Chars cs)    = show cs
  show (List exs)    = show exs
  show (Quot e)      = "'" ++ show e
  show (App e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lam s e)     = "(\\" ++ show s ++ ". " ++ show e ++ ")"
  show (Let s e1 e2) = "Let " ++ show s ++ " " ++ show e1 ++ " " ++ show e2
  show (Def s _)     = s
  show (Id s)        = s
  show (Cons s)      = s
  show (Data n _ _)  = n
  show (PAbs cs)     = "patterns: " ++ show cs
  show (PApp cs)     = "(" ++ show cs ++ ") "

match :: Pattern -> Expr -> Maybe [(String, Expr)]
match (App (Cons cons1) (Id r1)) (App (Cons cons2) r2)
  | cons1 == cons2 = Just [(r1, r2)]
  | otherwise      = Nothing
match (App l1 (Id r1)) (App l2 r2)
  = fmap ((r1, r2) :) (match l1 l2)
match (Id p) x
  = Just [(p, x)]
match a b
  | a == b    = Just []
  | otherwise = Nothing

parseCode :: String -> [Expr]
parseCode c = case runParser parser c of
  [] -> []
  ss -> fst . head $ ss
  where parser = many whitespace >> sepBy (many whitespace) expr

parseExpr :: String -> Maybe Expr
parseExpr s = case runParser expr s of
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
expr = atom 
   +++ quote 
   +++ function 
   +++ lambda 
   +++ def 
   +++ dataDef 
   +++ letExpr 
   +++ app 
   +++ list

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
atom = numLit <|>  cons <|> name <|> stringLit

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

name :: Parser Expr
name = Id <$> nameS

cons :: Parser Expr
cons = Cons <$> fmap (:[]) upper ||> many (letter +++ digit +++ special)

nameS :: Parser String
nameS = some (letter +++ digit +++ special)

brackets :: Char -> Char -> Parser a -> Parser a
brackets open close p = do
  char open
  many whitespace
  p' <- p
  many whitespace
  char close
  return p'

lambda :: Parser Expr
lambda = brackets '(' ')' $ do
  string "\\"
  many whitespace
  is   <-  sepBy (some whitespace) expr
  many whitespace
  char '.'
  many whitespace
  e    <- expr
  return $ foldr Lam e is

function :: Parser Expr
function = brackets '(' ')' $ do
  patterns <- sepBy (some whitespace) pattern
  return $ PAbs patterns

pattern :: Parser Expr
pattern = do
  is <- brackets '[' ']' $ sepBy (some whitespace) expr
  many whitespace
  e  <- expr
  return $ foldr Lam e is

letExpr :: Parser Expr
letExpr = brackets '(' ')' $ do
  string "let"
  some whitespace
  i <- expr
  some whitespace
  e    <- expr 
  some whitespace
  e'   <- expr
  return $ Let i e e'

def :: Parser Expr
def = brackets '(' ')' $ do
  string "def"
  some whitespace
  Id i <- name
  some whitespace
  e    <- expr 
  return $ Def i e

dataDef :: Parser Expr
dataDef = brackets '(' ')' $ do
  string "data"
  some whitespace
  char '('
  many whitespace
  Id i  <- name
  tvars <- (some whitespace >> sepBy (some whitespace) nameS) <|> return []
  many whitespace
  char ')'
  many whitespace
  c  <- sepBy (many whitespace) consDef
  return $ Data i tvars c

consDef :: Parser (String, [Expr])
consDef = brackets '[' ']' $ do
  Cons i  <- cons 
  tvars <- (some whitespace >> sepBy (some whitespace) name) <|> return []
  return (i, tvars)
