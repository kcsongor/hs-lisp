{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

import Evaluator
import Control.Applicative

import Data.IORef

main :: IO ()
main = do 
  i@ProgramState{..} <- ProgramState <$> newIORef 0
  putStrLn "Welcome to my simple lisp REPL!"
  runEval repl i 
  c <- readIORef counter
  print c

