{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Evaluator
import Control.Applicative
import Control.Monad

main :: IO ()
main = do 
  state <- ProgramState <$> return 0
  putStrLn "Welcome to my simple lisp REPL!"
  void $ runEval repl state
