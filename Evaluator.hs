{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator(
  Evaluator,
  ImpureState(..),
  runEval,
  PureState(..),
  repl,
  runCode,
  coreEnv,
) where

import Language
import Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Arrow

import Data.IORef
import Data.Maybe

import qualified Data.Map.Strict as M

data EvalError = PatternMatchE (Maybe String) 
               | TypeExistsE String
               | ConstructorExistsE String
               | TypeCheckE String 
               | ParserE String 
               deriving (Show)

data ImpureState = ImpureState {
  counter :: IORef Int -- Just a dummy for now
}

data PureState = PureState {
  evalEnv :: M.Map String Expr,
  typeEnv :: TEnv
}

runEval :: PureState -> ImpureState  -> Evaluator a -> IO (Either EvalError a, PureState)
runEval s r = (`runStateT` s) . (`runReaderT` r) . runExceptT

-- IO needed for REPL
type Evaluator a = ExceptT EvalError (ReaderT ImpureState (StateT PureState IO)) a 

--------------------------------------------------------------------------------
coreEnv :: TEnv
coreEnv = TEnv $ M.fromList $ map (second emptyScheme)
  [("+",    TFun TInt (TFun TInt TInt)),
   ("*",    TFun TInt (TFun TInt TInt)),
   ("-",    TFun TInt (TFun TInt TInt)),
   ("==",   TFun (TVar "a") (TFun (TVar "a") (TC "Bool" []))),
   ("eval", TFun (TVar "a") (TVar "a"))]

runCode :: String -> Evaluator ()
runCode code = forM_ exprs runExpr
  where exprs = parseCode code

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

deepEval :: Expr -> Evaluator Expr
deepEval (Lam x v) = do
  x' <- deepEval x
  v'  <- deepEval v
  return $ Lam x' v'
deepEval x = do
  x' <- eval x
  if x == x' then return x else deepEval x'

eval :: Expr -> Evaluator Expr
eval (App (App f@(Id "+") a1) a2)
  = do n1 <- eval a1
       n2 <- eval a2
       case (n1, n2) of
        (Number n1', Number n2') -> return $ Number (n1' + n2')
        (a1', a2')               -> return $ App (App f a1') a2'
eval (App (App f@(Id "*") a1) a2)
  = do n1 <- eval a1
       n2 <- eval a2
       case (n1, n2) of
        (Number n1', Number n2') -> return $ Number (n1' * n2')
        (a1', a2')               -> return $ App (App f a1') a2'
eval (App (App f@(Id "-") a1) a2)
  = do n1 <- eval a1
       n2 <- eval a2
       case (n1, n2) of
        (Number n1', Number n2') -> return $ Number (n1' - n2')
        (a1', a2')               -> return $ App (App f a1') a2'
eval (App (App (Id "==") a1) a2)
  = do a1' <- deepEval a1
       a2' <- deepEval a2
       return . Id $ if a1' == a2' then "True" else "False"
eval (App (Id "eval") (Quot q))
  = eval q 
eval (App (Id "eval") e)
  = do e' <- eval e
       return $ App (Id "eval") e'
eval (App (Lam x e1) e2)
  = do s@PureState{..} <- get
       let env' = M.fromList (fromJust m)
           s'   = s{ evalEnv = M.union env' evalEnv }
       when (isNothing m) . throwError $ PatternMatchE (Just . show $ x)
       put s'
       e1' <- deepEval e1
       put s
       return e1'
    where m = match x e2
eval (App (PAbs []) _)
  = throwError $ PatternMatchE Nothing
eval (App (PAbs (p : ps)) e)
  = eval (App p e) `catchError` handler
  -- If the patterns did not match, try the next pattern
  where handler (PatternMatchE _) = eval (App (PAbs ps) e)
        handler err = throwError err
eval (App e1 e2)
  = do e1' <- deepEval e1
       e2' <- deepEval e2
       return $ App e1' e2'
eval (Let x v i) = eval (App (Lam x i) v)
eval (Def x expr)
  = do s@PureState{..} <- get
       put s{ evalEnv = M.insert x expr evalEnv }
       return . Quot $ Id x
eval (Data n ts cs)
  = do s@PureState{..} <- get
       let cs'      = M.fromList $ map (second (generalise typeEnv . cons)) cs
           TEnv te' = typeEnv
           e_type = M.lookup n te'
           e_cons = M.intersection te' cs'
       -- see if type already exists $ TODO: types need to be stored
       when (isJust e_type) . throwError $
         TypeExistsE (show . fromJust $ e_type)
       -- see if constructor already exists
       when (M.size e_cons > 0) . throwError $
         ConstructorExistsE (show . head . fsts $ e_cons)
       put s{ typeEnv = TEnv $ M.union te' cs'}
       return . Quot $ Id n
    where constype = TC n (map TVar ts)
          cons     = foldr (\(Id s) -> TFun (TVar s)) constype
          fsts m   = map fst (M.toList m)
eval (Id x)
  = do PureState{..} <- get
       case M.lookup x evalEnv of
         Just x' -> deepEval x'
         Nothing -> return $ Id x
eval (List l)
  = do vs <- mapM eval l
       return $ List vs
eval (Quot q)
  = return $ Quot q
eval v = return v
