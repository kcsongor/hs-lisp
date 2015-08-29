{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Parser
import Control.Applicative
import Control.Monad

data Code = List [Code]
          | Id String 
          | If Code Code Code
          deriving (Show)

main :: IO ()
main = putStrLn "Welcome to my simple lisp REPL!" >> repl

-- TODO: put in a monad stack instead of plain IO
-- currently only serves as a parser
repl :: IO ()
repl = do
  putStr ">> "
  line <- getLine
  when (line /= ":q") $ do
    print $ runParser list line
    repl

list :: Parser Code
list = do
  many whitespace
  char '('
  many whitespace
  s <- sepBy (some whitespace) (ifExpr <|> symbol <|> list)
  many whitespace
  char ')'
  many whitespace
  List <$> return s

symbol :: Parser Code
symbol = Id <$> some (letter <|> digit <|> special)

ifExpr :: Parser Code
ifExpr = do
  string "if"
  c <- list
  many whitespace
  t <- list
  many whitespace
  f <- list
  return $ If c t f
