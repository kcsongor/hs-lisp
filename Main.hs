{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

import Evaluator
import Control.Applicative

import Data.IORef
import qualified Data.Map.Strict as M

main :: IO ()
main = do 
  iProg @ ProgramState{..} <- ProgramState <$> newIORef 0
  iEval @ EvalState{..}    <- return $ EvalState M.empty
  putStrLn "Welcome to my simple lisp REPL!"
  runEval iEval iProg repl
  c <- readIORef counter
  print c
