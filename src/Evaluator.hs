{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator(
  Evaluator,
  ImpureState(..),
  runEval,
  PureState(..),
  EvalError(..),
  coreEnv,
  eval,
  deepEval
) where

import Language
import Types

import Control.Arrow
import Control.Monad
import Control.Monad.Except
--import Control.Monad.Reader
--import Control.Monad.State
import Control.Monad.Trans.RWS.Strict

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

runEval :: PureState -> ImpureState  -> Evaluator a -> Either EvalError (a, PureState, String)
runEval s r = runExcept . (\evaluator -> runRWST evaluator r s)

type Evaluator a = RWST ImpureState String PureState (Except EvalError) a

--------------------------------------------------------------------------------
coreEnv :: TEnv
coreEnv = TEnv $ M.fromList $ map (second emptyScheme)
  [("+",    TFun TInt (TFun TInt TInt)),
   ("*",    TFun TInt (TFun TInt TInt)),
   ("-",    TFun TInt (TFun TInt TInt)),
   ("==",   TFun (TVar "a") (TFun (TVar "a") (TCons "Bool" []))),
   ("++",   TFun (TList (TVar "a")) (TFun (TList (TVar "a")) (TList (TVar "a")))),
   (":",   TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))),
   ("eval", TFun (TVar "a") (TVar "a"))]

deepEval :: Expr -> Evaluator Expr
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
eval (App (App f@(Id "++") a1) a2)
  = do l1 <- eval a1
       l2 <- eval a2
       case (l1, l2) of
        (List l1', List l2') -> return $ List (l1' ++ l2')
        (a1', a2')           -> return $ App (App f a1') a2'
eval (App (App f@(Id ":") a1) a2)
  = do l1 <- eval a1
       l2 <- eval a2
       case (l1, l2) of
        (l1', List l2') -> return $ List (l1' : l2')
        (a1', a2')      -> return $ App (App f a1') a2'
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
       return . Cons $ if a1' == a2' then "True" else "False"
eval (App (Id "eval") (Quot q))
  = eval q 
eval (App (Id "eval") e)
  = do e' <- eval e
       return $ App (Id "eval") e'
-- Lambdas are functions with a single pattern
eval (App (Lam x e1) e2)
  = return $ App (PAbs [Lam x e1]) e2
eval (App (PAbs []) _)
  = throwError $ PatternMatchE Nothing
eval (App (PAbs ps) e)
  = return $ App (PApp (map (\x -> (x, [])) ps)) e
eval (App e1 e2)
  = do e1' <- deepEval e1
       e2' <- deepEval e2
       case e1' of
         (PApp ps) -> do 
             let (PApp p) = PApp (mapMaybe (f e2') ps)
             finished <- final p
             case finished of
               (Just (expr, mappings)) -> do
                 s@PureState{..} <- get
                 let env' = M.fromList mappings
                     s'   = s{ evalEnv = M.union env' evalEnv }
                 put s'
                 expr' <- eval expr
                 put s
                 return expr'
               (Nothing)   -> return $ PApp p
         _         -> return $ App e1' e2'
  where f expr (Lam w t, e') = match w expr >>= \m' -> Just (t, m' ++ e')
        f _ l = Just l
        final [] = throwError $ PatternMatchE Nothing
        final ((Lam _ _, _) : _) = return Nothing
        final (p : _)       = return (Just p)
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
    where constype = TCons n (map TVar ts)
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
