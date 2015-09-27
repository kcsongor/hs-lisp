{-# LANGUAGE RecordWildCards #-}

module Interpreter (
  runREPL
) where

import Evaluator
import Language
import Types

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import qualified Data.Map as M
import qualified Data.Foldable as F (mapM_)

runREPL :: String -> Maybe String -> IO ()
runREPL corelib usercode = do
  let p = PureState M.empty coreEnv
  i@ ImpureState{..} <- ImpureState <$> newIORef 0
  void $ runEval p i $ do 
    runCode corelib
    F.mapM_ runCode usercode
    repl

repl :: Evaluator ()
repl = do
  liftIO $ putStr ">> "
  line <- liftIO getLine
  when (line /= ":q") $ do 
    ImpureState{..} <- ask
    liftIO $ modifyIORef counter (+1)
    case parseExpr line of
      Just code -> runExpr code
      Nothing -> throwError $ ParserE "Couldn't parse"
    repl
  -- print the error, then continue
  `catchError` (\e -> void $ liftIO (putStrLn $ "Error: " ++ show e) >> repl)


runCode :: String -> Evaluator ()
runCode code =
  case exprs of 
    []     -> throwError $ ParserE "Loaded code couldn't be parsed"
    exprs' -> forM_ exprs' runExpr
  `catchError` (\e -> void $ liftIO (putStrLn $ "Error: " ++ show e))
  where exprs = parseCode code

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
      liftIO . putStrLn $ show e ++ " :: " ++ show t'
