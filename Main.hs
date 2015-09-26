{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

import Evaluator

import Control.Applicative
import Data.IORef
import qualified Data.Foldable as F

import qualified Data.Map.Strict as M
import System.Environment

welcome :: IO ()
welcome = putStrLn "REPL 0.1"

loadCode :: IO (Maybe String)
loadCode = do
  args <- getArgs
  case args of
   (a : _) -> Just <$> readFile a
   _       -> return Nothing

main :: IO ()
main = do 
  let p = PureState M.empty coreEnv
  i @ ImpureState{..} <- ImpureState <$> newIORef 0
  corelib  <- readFile "corelib.code"
  usercode <- loadCode
  welcome
  runEval p i $ do 
    runCode corelib
    F.forM_ usercode runCode
    repl
  c <- readIORef counter
  print c
