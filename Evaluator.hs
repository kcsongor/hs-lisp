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

data ImpureState = ImpureState {
  counter :: IORef Int -- Just a dummy for now
}

data PureState = PureState {
  evalEnv :: M.Map String Expr,
  typeEnv :: TEnv
}

runEval :: PureState -> ImpureState  -> Evaluator a -> IO (Either String a, PureState)
runEval s r = (`runStateT` s) . (`runReaderT` r) . runExceptT

-- IO needed for REPL
type Evaluator a = ExceptT String (ReaderT ImpureState (StateT PureState IO)) a 

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
      Nothing -> throwError "Couldn't parse"
    repl
  -- print the error, then continue
  `catchError` (\s -> void $ liftIO (putStrLn $ "Error: " ++ s) >> repl)

runExpr :: Expr -> Evaluator ()
runExpr code = do
  p@PureState{..} <- get
  let (tRes, inferEnv) = runTI typeEnv $ inferType code
  case tRes of
    Left err -> throwError err
    Right t' -> do
      let TEnv typeEnv'  = typeEnv
      let TEnv typeEnv'' = isEnv inferEnv
      put p{ typeEnv = TEnv $ M.union typeEnv' typeEnv'' }
      e <- deepEval code
      liftIO . putStrLn $ show e ++ " :: " ++ show t'

deepEval :: Expr -> Evaluator Expr
deepEval (Abs x v) = do
  x' <- deepEval x
  v'  <- deepEval v
  return $ Abs x' v'
deepEval x = do
  x' <- eval x
  if x == x' then return x else deepEval x'

-- | TODO:
-- define most of the functions using the language itself (implement pattern matching)
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
       return $ Boolean (a1' == a2')
eval (App (App f@(Id "or") a1) a2)
  = do n1 <- eval a1
       n2 <- eval a2
       case (n1, n2) of
        (Boolean n1', Boolean n2') -> return $ Boolean (n1' || n2')
        (a1', a2')                 -> return $ App (App f a1') a2'
eval (App (App f@(Id "and") a1) a2)
  = do n1 <- eval a1
       n2 <- eval a2
       case (n1, n2) of
        (Boolean n1', Boolean n2') -> return $ Boolean (n1' && n2')
        (a1', a2')                 -> return $ App (App f a1') a2'
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
       let env' = M.fromList (fromJust m)
           s'   = s{ evalEnv = M.union env' evalEnv }
       when (isNothing m) $ throwError $ "Irrefutable pattern: " ++ show x 
       put s'
       e1' <- deepEval e1
       put s
       return e1'
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
           e_type = M.lookup n te'
           e_cons = M.intersection te' cs'
       -- see if type already exists $ TODO: types need to be stored
       when (isJust e_type) $ throwError $
         "Type " ++ (show . fromJust $ e_type) ++ " is already defined"
       -- see if constructor already exists
       when (M.size e_cons > 0) $ throwError $
         "Constructor " ++ (show . head . fsts $ e_cons) ++ " is already defined"
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
