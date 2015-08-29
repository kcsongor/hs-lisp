import Parser
import Control.Applicative
import Control.Monad

data Code = List [Code]
          | Id String deriving (Show)

main :: IO ()
main = putStrLn "Welcome to my simple lisp REPL!" >> repl

-- TODO: put in a monad stack instead of plain IO
-- currently only serves as a parser
repl :: IO ()
repl = do
  putStr ">> "
  line <- getLine
  when (line /= ":q") $ do
    print $ runParser block line
    repl

block :: Parser Code
block = do
  _ <- char '('
  s <- sepBy (some whitespace) (symbol <|> block)
  _ <- char ')'
  List <$> return s

symbol :: Parser Code
symbol = Id <$> some (letter <|> digit)
