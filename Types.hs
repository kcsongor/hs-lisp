{-# LANGUAGE RecordWildCards #-}

module Types(
  Type(..),
  inferType,
  emptyEnv,
  runTI,
  runTypeInference,
) where

import Language

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data Type = TInt 
          | TBool
          | TList Type
          | TFun Type Type 
          | TVar String 
          deriving (Eq, Show)
data Scheme = Scheme [String] Type deriving (Show)

newtype TEnv = TEnv (M.Map String Scheme)

emptyEnv :: TEnv
emptyEnv = TEnv (M.empty)

type Sub = M.Map String Type

class TypeTable a where
  freeVars   :: a -> S.Set String
  substitute :: Sub -> a -> a

instance TypeTable Type where
  freeVars (TVar s)
    = S.singleton s
  freeVars (TFun f1 f2)
    = S.union (freeVars f1) (freeVars f2)
  freeVars _
    = S.empty
  substitute s (TVar v)
    = case M.lookup v s of
        Just t  -> t
        Nothing -> TVar v
  substitute s (TFun f1 f2)
    = TFun (substitute s f1) (substitute s f2)
  substitute s (TList l)
    = TList (substitute s l)
  substitute _ t
    = t

instance TypeTable Scheme where
  freeVars (Scheme vs t)
    = S.difference (freeVars t) (S.fromList vs)
  substitute s (Scheme vs t) -- delete all vs from s and apply that to t
    = Scheme vs (substitute (foldr M.delete s vs) t)

instance TypeTable a => TypeTable [a] where
  freeVars t = foldr S.union S.empty (map freeVars t)
  substitute s = map (substitute s)

instance TypeTable TEnv where
  freeVars (TEnv e)     = freeVars (M.elems e)
  substitute s (TEnv e) = TEnv (M.map (substitute s) e)

noSub :: Sub
noSub = M.empty

combine :: Sub -> Sub -> Sub
combine s1 s2 = M.union s1 (M.map (substitute s1) s2)

remove :: TEnv -> String -> TEnv
remove (TEnv e) s = TEnv (M.delete s e)

generalise :: TEnv -> Type -> Scheme
generalise e t = Scheme vars t
  where vars = S.toList (S.difference (freeVars t) (freeVars e))

----------------------------------------------------------------------------------

data InferState = InferState {
  isVars :: Int,
  isSub  :: Sub
} deriving (Show)

type TI a = ExceptT String (State InferState) a

runTI :: TI a -> (Either String a, InferState)
runTI = (`runState` initState) . runExceptT
  where initState = InferState { isVars = 0, isSub = noSub }

nextVar :: TI Type
nextVar = do st@InferState{..} <- get
             put st{ isVars = isVars + 1}
             return (TVar ('t' : show isVars))

refreshVars :: Scheme -> TI Type
refreshVars (Scheme vars t)
  = do newvars <- forM vars (const nextVar)
       let s = M.fromList $ zip vars newvars
       return $ substitute s t

bind :: String -> Type -> TI Sub
bind s t
  | t == TVar s
      = return noSub
  | S.member s (freeVars t)
      = throwError $ "Occur check failed: " ++ show t ++ ", " ++ s
  | otherwise
      = return (M.singleton s t)

unify :: Type -> Type -> TI Sub
unify (TFun l r) (TFun l' r')
  = do sl <- unify l l'
       sr <- unify (substitute sl r) (substitute sl r')
       return $ combine sl sr
unify (TVar v) t
  = bind v t
unify t (TVar v)
  = bind v t
unify (TList t1) (TList t2)
  = unify t1 t2
unify TInt TBool
  = return noSub
unify TBool TInt
  = return noSub
unify t1 t2
  = throwError $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

inferSub :: TEnv -> Expr -> TI (Sub, Type)
inferSub (TEnv e) (Id v)
  = case M.lookup v e of
      Nothing     -> throwError $ "unbound variable " ++ v
      Just scheme -> do t <- refreshVars scheme
                        return (noSub, t)
inferSub _ (Number _)
  = return (noSub, TInt)
inferSub _ (Boolean _)
  = return (noSub, TBool)
inferSub e (Abs l r)
  = do var <- nextVar
       let TEnv e' = remove e l
           env     = TEnv $ M.union e' (M.singleton l (Scheme [] var))
       (s, t) <- inferSub env r
       return (s, TFun (substitute s var) t)
inferSub e (App l r)
  = do var      <- nextVar
       (sl, tl) <- inferSub e l
       (sr, tr) <- inferSub (substitute sl e) r
       s        <- unify (substitute sr tl) (TFun tr var)
       return (foldr1 combine [s, sr, sl], substitute s var)
inferSub e (Let x expr1 expr2)
  = do (s1, t1) <- inferSub e expr1
       let TEnv e' = remove e x
           t'      = generalise (substitute s1 e) t1
           env     = TEnv (M.insert x t' e')
       (s2, t2) <- inferSub (substitute s1 env) expr2
       return (combine s1 s2, t2)
inferSub _ (List [])
  = do var <- nextVar
       return (noSub, TList var)
inferSub e (List [e1])
  = do (s1, t1) <- inferSub e e1
       return (s1, TList t1)
inferSub e (List (e1 : es))
  = do (s1, t1)         <- inferSub e e1
       (ss, (TList ts)) <- inferSub (substitute s1 e) (List es)
       s                <- unify t1 ts
       let t1' = substitute s t1
       if (t1' /= ts) 
         then throwError $ show t1' ++ " doesn't match list type: " ++ show ts
         else return (ss, TList ts)

inferType :: TEnv -> Expr -> TI Type
inferType env expr
  = do (sub, t) <- inferSub env expr
       return $ substitute sub t

runTypeInference :: TEnv -> Expr -> (Either String Type)
runTypeInference env expr
  = let (t, _) = runTI $ inferType env expr in t 
