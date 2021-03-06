module Parser(
  Parser,
  runParser,
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
  sepByOpt,
  whitespace,
  string,
  anyOf,
  noneOf,
  special,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Monoid

type B = []

type Parser a = StateT String B a

runParser :: Parser a -> String -> B (a, String)
runParser = runStateT

-- Combinators -----------------------------------------------------------------
(||>) :: Monoid a => Parser a -> Parser a -> Parser a
p1 ||> p2 = do
  x <- p1
  y <- p2
  return $ mappend x y

(+++) :: Parser a -> Parser a -> Parser a
(+++) = mplus

failure :: Parser a
failure = lift mzero

item :: Parser Char
item = do
  (x : xs) <- get
  put xs
  return x

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
number = do neg <- Just <$> char '-' <|> return Nothing
            n <- many digit
            case n of 
              [] -> failure
              _  -> case neg of 
                Just _  -> return $ (-1) * read n
                Nothing -> return $ read n

upper :: Parser Char
upper = anyOf ['A'..'Z']

lower :: Parser Char
lower = anyOf ['a'..'z']

letter :: Parser Char
letter = lower +++ upper

special :: Parser Char
special = anyOf "&~!@#$%^*-=_+?<>,/?;:"

word :: Parser String
word = many letter

whitespace :: Parser Char
whitespace = anyOf " \n\t"

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy pa pb = do
  b  <- pb
  b' <- many pb'
  return $ b : b'
  where pb' = do
          _ <- pa
          pb

sepByOpt :: Parser a -> Parser b -> Parser [b]
sepByOpt pa pb = sepBy pa pb <|> return []
