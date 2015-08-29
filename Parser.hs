{-# LANGUAGE DeriveFunctor #-}
module Parser where

import Control.Applicative
import Data.Monoid

newtype Parser a = Parser (String -> [(a, String)]) deriving (Functor)

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser p) = p

-- Instances -------------------------------------------------------------------
instance Applicative Parser where
  pure = return
  f <*> p1 = Parser $
    \s -> concat [runParser (fmap f' p1) s' | (f', s') <- runParser f s]

instance Alternative Parser where
  empty = zero
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      []     -> runParser p2 s
      result -> result
  -- | many and some we got for free

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  Parser p1 >>= f = Parser $
    \s -> concat [runParser (f x) s' | (x, s') <- p1 s]

-- Combinators -----------------------------------------------------------------
(>>>) :: Parser a -> Parser b -> Parser (a, b)
p1 >>> p2 = do
  x <- p1
  y <- p2
  return (x, y)

(||>) :: Monoid a => Parser a -> Parser a -> Parser a
p1 ||> p2 = do
  x <- p1
  y <- p2
  return $ mappend x y

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser $ \x -> case x of
                      []       -> []
                      (a : as) -> [(a, as)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else zero

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat (\c -> '0' <= c && c <= '9')

upper :: Parser Char
upper = sat (\c -> 'A' <= c && c <= 'Z')

lower :: Parser Char
lower = sat (\c -> 'a' <= c && c <= 'z')

letter :: Parser Char
letter = lower +++ upper

word :: Parser String
word = many letter

scope :: Parser String
scope = do
  b1      <- char '('
  content <- do 
    letters  <- many (letter <|> char ' ')
    bracks   <- many scope
    letters2 <- many (letter <|> char ' ')
    return $ letters ++ concat bracks ++ letters2
  b2      <- char ')'
  return $ [b1] ++ content ++ [b2]
