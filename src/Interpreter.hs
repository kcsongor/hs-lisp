{-# LANGUAGE RecordWildCards #-}

module Interpreter (
  runREPL
) where

import Evaluator
import Language
import Types

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.RWS.Strict
import Data.IORef
import qualified Data.Map as M
import qualified Data.Foldable as F (mapM_)

printError :: EvalError -> IO ()
printError err = putStrLn $ "Error: " ++ show err 

-- Load the core library, and maybe some user code
-- then start the REPL
runREPL :: String -> Maybe String -> IO ()
runREPL corelib usercode = do
  let p = PureState M.empty coreEnv
  i@ ImpureState{..} <- ImpureState <$> newIORef 0
  case runEval p i (action corelib usercode) of
    Left err         -> printError err
    Right (_, p', w) -> putStr w >> repl i p'
  where action c u = do
          runCode c
          F.mapM_ runCode u

repl :: ImpureState -> PureState -> IO ()
repl i p = do
  putStr ">> "
  line <- getLine
  when (line /= ":q") $
    case runEval p i (runCode line) of
      Left err -> do
        printError err
        repl i p
      Right (_, p', w) -> do
        putStr w
        repl i p'

-- Parses code then runs the expressions found
runCode :: String -> Evaluator ()
runCode code =
  case exprs of 
    []     -> throwError $ ParserE "Couldn't parse"
    exprs' -> forM_ exprs' runExpr
  where exprs = parseCode code

-- Runs parsed expressions
runExpr :: Expr -> Evaluator ()
runExpr code = do
  p@PureState{..} <- get
  let (tRes, inferEnv) = runTI typeEnv $ inferType code
  case tRes of
    Left err -> throwError $ TypeCheckE err
    Right t' -> do
      let TEnv typeEnv'  = typeEnv
      let TEnv typeEnv'' = isEnv inferEnv
      put p{ typeEnv = TEnv $ M.union typeEnv'' typeEnv' }
      e <- deepEval code
      tell $ show e ++ " :: " ++ show t' ++ "\n"
