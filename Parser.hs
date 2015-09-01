{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser(
  Parser,
  runParser,
  (>>>),
  (||>),
  (+++),
  failure,
  item,
  sat,
  char,
  digit,
  number,
  upper,
  lower,
  letter,
  word,
  sepBy,
  whitespace,
  string,
  anyOf,
  noneOf,
  special,
) where

import Control.Applicative
import Control.Monad

import Data.Monoid

-- TODO: parser should be a State monad
newtype Parser a 
  = Parser { runParser :: String -> [(a, String)]} deriving (Functor)

-- Instances -------------------------------------------------------------------
instance Applicative Parser where
  pure = return
  (<*>) = ap

-- | gives 'many' and 'some' for free
instance Alternative Parser where
  empty = failure 
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      []     -> runParser p2 s
      result -> result

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  p1 >>= f = Parser $
    \s -> concat [runParser (f x) s' | (x, s') <- runParser p1 s]

instance MonadPlus Parser where
  mzero = failure
  mplus = (+++)

-- Combinators -----------------------------------------------------------------
(>>>) :: Parser a -> Parser b -> Parser (a, b)
p1 >>> p2 = do
  x <- p1
  y <- p2
  return (x, y)

-- Not sure if this is needed
(||>) :: Monoid a => Parser a -> Parser a -> Parser a
p1 ||> p2 = do
  x <- p1
  y <- p2
  return $ mappend x y

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s

failure :: Parser a
failure = Parser $ const []

item :: Parser Char
item = Parser $ \x -> case x of
                      []       -> []
                      (a : as) -> [(a, as)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure 

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (c : cs) = do
  x  <- char c
  xs <- string cs
  return $ x : xs

anyOf :: String -> Parser Char
anyOf xs = sat (`elem` xs)

noneOf :: String -> Parser Char
noneOf xs = sat (`notElem` xs)

digit :: Parser Char
digit = anyOf ['0'..'9']

number :: (Read a, Num a) => Parser a
number = do n <- many digit
            case n of 
              [] -> failure
              _  -> return $ read n

upper :: Parser Char
upper = anyOf ['A'..'Z']

lower :: Parser Char
lower = anyOf ['a'..'z']

letter :: Parser Char
letter = lower +++ upper

special :: Parser Char
special = anyOf "~!@#$%^&*-=_+?[]{}<>,./?;"

word :: Parser String
word = many letter

whitespace :: Parser Char
whitespace = sat (\c -> c == ' ' || c == '\n' || c == '\t')

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy pa pb = do
  b  <- pb
  b' <- many pb'
  return $ b : b'
  where pb' = do
          _ <- pa
          pb
