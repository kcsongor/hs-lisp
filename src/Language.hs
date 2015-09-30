{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language(
  Expr(..),
  parseExpr,
  parseCode,
  parseNum,
  match,
  boolToExpr,
) where

import Parser
import Control.Applicative

type Pattern = Expr
type Match = (String, Expr)
data Expr = Number Int
          | Char Char
          | Id String 
          | Cons String 
          | List [Expr]
          | Quot Expr
          | App Expr Expr
          | Lam Expr Expr
          -- List of Lams
          | PAbs [Expr]
          -- intermediate representation for pattern matching
          | PApp [(Expr, [Match])] -- fst: the leftover, snd: mappings of args to their values
          | Let Expr Expr Expr
          | Def String Expr
          | Data String [String] [(String, [Expr])]
          deriving (Eq)

instance Show Expr where
  show (Number n)     = show n
  show (Char c)       = show c
  show (List exs)     = case exs of
                          (Char _ : _) -> show $ map (\(Char c) -> c) exs
                          l -> show l
  show (Quot e)       = "'" ++ show e
  show (App e1 e2)    = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lam s e)      = "(\\" ++ show s ++ ". " ++ show e ++ ")"
  show (Let s e1 e2)  = "Let " ++ show s ++ " " ++ show e1 ++ " " ++ show e2
  show (Def s _)      = s
  show (Id s)         = s
  show (Cons s)       = s
  show (Data n _ _)   = n
  show (PAbs cs)      = "patterns: " ++ show cs
  show (PApp cs)      = "(" ++ show cs ++ ") "

boolToExpr :: Bool -> Expr
boolToExpr False = Cons "False"
boolToExpr True  = Cons "True"

match :: Pattern -> Expr -> Maybe [(String, Expr)]
match (App (App (Id ":") (Id h)) m') (List l) =
  case l of 
    [] -> Nothing
    (x : xs) -> match m' (List xs) >>= \ps -> Just ((h, x) : ps)
match (App (App (Id "++") (List l1)) rest) (List l2) =
  case l2 of 
    [] -> Nothing
    _  -> do 
      (lms, r) <- matchList l1 l2
      ms <- match rest r
      return (lms ++ ms)
  where matchList (l1' : l1s') (l2' : l2s') = do
          ms <- match l1' l2'
          (lms, r) <- matchList l1s' l2s'
          return (ms ++ lms, r)
        matchList [] rest'
          = Just ([], List rest')
        matchList _ _
          = Nothing
match (List (l1 : l1s)) (List (l2 : l2s))
  = match l1 l2 >>= \ms -> fmap (ms ++) (match (List l1s) (List l2s))
match (List []) (List []) = Just []
match (App (Cons cons1) r1) (App (Cons cons2) r2)
  | cons1 == cons2 = match r1 r2
  | otherwise      = Nothing
match (App l1 r1) (App l2 r2)
  = match r1 r2 >>= \m -> match l1 l2 >>= \m' -> Just $ m ++ m'
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
  s <- sepByOpt (some whitespace) expr
  many whitespace
  return s

atom :: Parser Expr
atom = numLit <|>  cons <|> name <|> stringLit <|> charLit

numLit :: Parser Expr
numLit = do
  n <- number
  return . Number $ n

stringLit :: Parser Expr
stringLit = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return . List $ map Char s

charLit :: Parser Expr
charLit = do
  char '\''
  c <- item
  char '\''
  return . Char $ c

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
  lamRight is

function :: Parser Expr
function = brackets '(' ')' $ do
  patterns <- sepBy (some whitespace) pattern
  return $ PAbs patterns

pattern :: Parser Expr
pattern = do
  is <- brackets '[' ']' $ sepBy (some whitespace) expr
  many whitespace
  lamRight is

lamRight :: [Expr] -> Parser Expr
lamRight is = do
  e <- expr
  return $ foldr Lam e is

letExpr :: Parser Expr
letExpr = brackets '(' ')' $ do
  string "let"
  some whitespace
  as <- brackets '(' ')' (sepBy (many whitespace) assignment) 
        <|> fmap (:[]) assignment
  some whitespace
  i  <- expr
  return $ foldr (uncurry Let) i as
  where assignment
          = brackets '(' ')' $ do
              e <- expr
              some whitespace
              v <- expr
              return (e, v)

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
