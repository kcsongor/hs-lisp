{-# LANGUAGE DeriveFunctor #-}
module Parser where

import Control.Applicative

newtype Parser a = Parser (String -> [(a, String)]) deriving (Functor)

instance Applicative Parser where
  pure = return
  f <*> p1 
    = Parser $ \s -> concat [runParser (fmap f' p1) s' | (f', s') <- runParser f s]

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  Parser p1 >>= f 
    = Parser $ \s -> concat [runParser (f x) s' | (x, s') <- p1 s]

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser p) = p

-- Combinators -----------------------------------------------------------------
(>>>) :: Parser a -> Parser b -> Parser (a, b)
p1 >>> p2 = do
  x <- p1
  y <- p2
  return (x, y)

plus :: Parser a -> Parser a -> Parser a
p1 `plus` p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s

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
