{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

import Evaluator
import Control.Applicative

import Data.IORef
import qualified Data.Map.Strict as M

main :: IO ()
main = do 
  let p = PureState M.empty coreEnv
  i @ ImpureState{..} <- ImpureState <$> newIORef 0
  putStrLn "Welcome to my simple lisp REPL!"
  runEval p i repl
  c <- readIORef counter
  print c
