{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

import Evaluator
import Control.Applicative

import Data.IORef

main :: IO ()
main = do 
  iProg @ ProgramState{..} <- ProgramState <$> newIORef 0
  iEval @ EvalState{..}    <- return EvalState
  putStrLn "Welcome to my simple lisp REPL!"
  runEval iEval iProg repl
  c <- readIORef counter
  print c
