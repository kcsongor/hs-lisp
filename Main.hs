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
  runEval p i (runCode code >> repl)
  c <- readIORef counter
  print c
  where code = "(data (Maybe a) [Just a] [Nothing])\
                \(def fromJust (\\(Just x).x))\
                \(def fmap (\\f (Just x). (Just (f x))))\
                \(data (Bool) [True] [False])\
                \(def or (\
                \  [True x]  True\
                \  [False x] x\
                \))\
                \(def and (\
                \  [True x]  x\
                \  [False x] False\
                \))\
                \(def if (\
                \  [True a b]  a\
                \  [False a b] b\
                \))"
