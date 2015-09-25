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
  runEval p i (runCode "(data (Maybe a) [Just a] [Nothing]) (def fromJust (\\(Just x).x))" >> repl)
  c <- readIORef counter
  print c
