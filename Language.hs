{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language(
  Code,
  parseString,
) where

import Parser
import Control.Applicative

data Code = List [Code]
          | Id String 
          | If Code Code Code
          deriving (Show)

parseString :: String -> Maybe Code
parseString s = case runParser list s of
  [] -> Nothing
  ss -> Just $ fst . head $ ss

list :: Parser Code
list = do
  char '('
  --many whitespace
  s <- sepBy (some whitespace) (ifExpr <|> symbol <|> list)
  --many whitespace
  char ')'
  List <$> return s

symbol :: Parser Code
symbol = Id <$> some (letter <|> digit <|> special)

ifExpr :: Parser Code
ifExpr = do
  string "if"
  some whitespace
  lists <- sepBy (many whitespace) list
  case lists of
    [c, t, f] -> return $ If c t f
    _         -> failure
