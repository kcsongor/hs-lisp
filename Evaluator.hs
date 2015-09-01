{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator(
  Evaluator,
  ProgramState(..),
  runEval,
  EvalState(..),
  repl,
) where

import Language
import Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.IORef

import qualified Data.Map.Strict as M

data ProgramState = ProgramState {
  counter :: IORef Int
}

data EvalState = EvalState {
}

runEval :: EvalState -> ProgramState -> Evaluator a -> IO (a, EvalState)
runEval s r = (`runStateT` s) . (`runReaderT` r)

-- IO needed for REPL
type Evaluator a = ReaderT ProgramState (StateT EvalState IO) a 

--------------------------------------------------------------------------------
coreEnv :: TEnv
coreEnv = TEnv $ M.fromList
  [("+",    Scheme [] (TFun TInt (TFun TInt TInt))),
   ("-",    Scheme [] (TFun TInt (TFun TInt TInt))),
   ("==",   Scheme [] (TFun (TVar "a") (TFun (TVar "a") TBool))),
   ("if",   Scheme [] (TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a"))))),
   ("and",  Scheme [] (TFun TBool (TFun TBool TBool))),
   ("or",   Scheme [] (TFun TBool (TFun TBool TBool))),
   -- TODO: quotes should have their own type (or should they?)
   ("eval", Scheme [] (TFun (TVar "a") (TVar "a")))]

repl :: Evaluator ()
repl = do
  liftIO $ putStr ">> "
  line <- liftIO getLine
  when (line /= ":q") $ do 
    ProgramState{..} <- ask
    liftIO $ modifyIORef counter (+1)
    case parseString line of
      Just code ->
        case runTypeInference coreEnv code of
          Left err -> liftIO $ putStrLn err
          Right t' -> do
            liftIO $ print t'
            e <- eval code
            liftIO $ print e
      Nothing -> liftIO $ putStrLn "couldn't parse"
    repl

-- | TODO:
-- define most of the functions using the language itself (implement pattern matching)
eval :: Expr -> Evaluator Expr
eval (App (App (Id "+") a1) a2)
  = do Number n1 <- eval a1
       Number n2 <- eval a2
       return $ Number (n1 + n2)
eval (App (App (Id "-") a1) a2)
  = do Number n1 <- eval a1
       Number n2 <- eval a2
       return $ Number (n1 - n2)
eval (App (App (Id "==") a1) a2)
  = do a1' <- eval a1
       a2' <- eval a2
       return $ Boolean (a1' == a2')
eval (App (App (Id "or") a1) a2)
  = do Boolean a1' <- eval a1
       Boolean a2' <- eval a2
       return $ Boolean (a1' || a2')
eval (App (App (Id "and") a1) a2)
  = do Boolean a1' <- eval a1
       Boolean a2' <- eval a2
       return $ Boolean (a1' && a2')
eval (App (App (App (Id "if") p) t) f)
  = do Boolean p' <- eval p
       if p' then eval t else eval f
eval (List l)
  = do vs <- mapM eval l
       return $ List vs
eval (Quot q)
  = return $ Quot q
eval (App (Id "eval") (Quot q))
  = eval q 
eval v = return v
