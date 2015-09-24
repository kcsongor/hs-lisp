{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator(
  Evaluator,
  ImpureState(..),
  runEval,
  PureState(..),
  repl,
  coreEnv,
) where

import Language
import Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow

import Data.IORef

import qualified Data.Map.Strict as M

data ImpureState = ImpureState {
  counter :: IORef Int -- Just a dummy for now
}

data PureState = PureState {
  evalEnv :: M.Map String Expr,
  typeEnv :: TEnv
}

runEval :: PureState -> ImpureState  -> Evaluator a -> IO (a, PureState)
runEval s r = (`runStateT` s) . (`runReaderT` r)

-- IO needed for REPL
type Evaluator a = ReaderT ImpureState (StateT PureState IO) a 

--------------------------------------------------------------------------------
coreEnv :: TEnv
coreEnv = TEnv $ M.fromList $ map (second emptyScheme)
  [("+",    TFun TInt (TFun TInt TInt)),
   ("*",    TFun TInt (TFun TInt TInt)),
   ("-",    TFun TInt (TFun TInt TInt)),
   ("==",   TFun (TVar "a") (TFun (TVar "a") TBool)),
   ("if",   TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))),
   ("and",  TFun TBool (TFun TBool TBool)),
   ("or",   TFun TBool (TFun TBool TBool)),
   ("eval", TFun (TVar "a") (TVar "a"))]

repl :: Evaluator ()
repl = do
  liftIO $ putStr ">> "
  line <- liftIO getLine
  when (line /= ":q") $ do 
    p@PureState{..} <- get
    ImpureState{..} <- ask
    liftIO $ modifyIORef counter (+1)
    case parseString line of
      Just code -> do
        let (tRes, inferEnv) = runTI typeEnv $ inferType code
        case tRes of
          Left err -> liftIO $ putStrLn err
          Right t' -> do
            let TEnv typeEnv'  = typeEnv
            let TEnv typeEnv'' = isEnv inferEnv
            put p{ typeEnv = TEnv $ M.union typeEnv' typeEnv'' }
            e <- deepEval code
            liftIO . putStrLn $ show e ++ " :: " ++ show t'
      Nothing -> liftIO $ putStrLn "couldn't parse"
    repl

deepEval :: Expr -> Evaluator Expr
deepEval x = do
  x' <- eval x
  if x == x' then return x else deepEval x'

-- | TODO:
-- define most of the functions using the language itself (implement pattern matching)
eval :: Expr -> Evaluator Expr
eval (App (App (Id "+") a1) a2)
  = do Number n1 <- eval a1
       Number n2 <- eval a2
       return $ Number (n1 + n2)
eval (App (App (Id "*") a1) a2)
  = do Number n1 <- eval a1
       Number n2 <- eval a2
       return $ Number (n1 * n2)
eval (App (App (Id "-") a1) a2)
  = do Number n1 <- eval a1
       Number n2 <- eval a2
       return $ Number (n1 - n2)
eval (App (App (Id "==") a1) a2)
  = do a1' <- deepEval a1
       a2' <- deepEval a2
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
eval (App (Id "eval") (Quot q))
  = eval q 
eval (App (Id "eval") e)
  = do e' <- eval e
       return $ App (Id "eval") e'
eval (App (Abs x e1) e2)
  = do s@PureState{..} <- get
       case m of
         Just mapping -> do
            let env' = M.fromList mapping
                s'   = s{ evalEnv = M.union env' evalEnv }
            st <- ask
            (e1', _) <- liftIO $ runEval s' st (eval e1)
            return e1'
         Nothing -> return (Id "TODO: pattern fail")
    where m = match x e2
eval (App e1 e2)
  = do e1' <- deepEval e1
       e2' <- deepEval e2
       return $ App e1' e2'
eval (Let x v i) = eval (App (Abs x i) v)
eval (Def x expr)
  = do s@PureState{..} <- get
       put s{ evalEnv = M.insert x expr evalEnv }
       return . Quot $ Id x
eval (Data n ts cs)
  = do s@PureState{..} <- get
       let cs'      = M.fromList $ map (second (generalise typeEnv . cons)) cs
           TEnv te' = typeEnv
       put s{ typeEnv = TEnv $ M.union te' cs'}
       return . Quot $ Id n
    where constype = TC n (map TVar ts)
          cons     = foldr (\(Id s) -> TFun (TVar s)) constype
eval (Id x)
  = do PureState{..} <- get
       case M.lookup x evalEnv of
         Just x' -> eval x'
         Nothing -> return $ Id x
eval (List l)
  = do vs <- mapM eval l
       return $ List vs
eval (Quot q)
  = return $ Quot q
eval v = return v
