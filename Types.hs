{-# LANGUAGE RecordWildCards #-}

module Types(
  inferType,
  emptyEnv,
  Scheme(..),
  TEnv(..),
  Type(..),
  InferState(..),
  runTI,
  runTypeInference,
  emptyScheme,
  generalise,
) where

import Language

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

-- TODO: type literals for data type declaration
data Type = TInt 
          | TChar
          | TC String [Type]
          | TList Type
          | TFun Type Type 
          | TVar String 
          deriving (Eq, Ord)

instance Show Type where
  show TInt  
    = "Int"
  show TChar 
    = "Char"
  show (TFun t t') 
    = "(" ++ show t ++ " -> " ++ show t' ++ ")"
  show (TList TChar)
    = "String"
  show (TList t)
    = "[" ++ show t ++ "]"
  show (TVar a) 
    = a
  show (TC n ts)
    = n ++ " (" ++ (unwords . map show $ ts) ++ ")"

data Scheme = Scheme [String] Type deriving (Show)

newtype TEnv = TEnv (M.Map String Scheme)

emptyEnv :: TEnv
emptyEnv = TEnv M.empty

type Sub = M.Map String Type

class TypeTable a where
  freeVars   :: a -> S.Set String
  substitute :: Sub -> a -> a

instance TypeTable Type where
  freeVars (TVar s)
    = S.singleton s
  freeVars (TFun f1 f2)
    = S.union (freeVars f1) (freeVars f2)
  freeVars (TList l)
    = freeVars l
  freeVars (TC _ ts)
    = freeVars ts
  freeVars _
    = S.empty
  substitute s (TVar v)
    = fromMaybe (TVar v) (M.lookup v s)
  substitute s (TFun f1 f2)
    = TFun (substitute s f1) (substitute s f2)
  substitute s (TList l)
    = TList (substitute s l)
  substitute s (TC n ts)
    = TC n (substitute s ts)
  substitute _ t
    = t

instance TypeTable Scheme where
  freeVars (Scheme vs t)
    = S.difference (freeVars t) (S.fromList vs)
  substitute s (Scheme vs t) -- delete all vs from s and apply that to t
    = Scheme vs (substitute (foldr M.delete s vs) t)

instance TypeTable a => TypeTable [a] where
  freeVars = foldr (S.union . freeVars) S.empty
  substitute s = map (substitute s)

instance TypeTable TEnv where
  freeVars (TEnv e)     = freeVars (M.elems e)
  substitute s (TEnv e) = TEnv (M.map (substitute s) e)

noSub :: Sub
noSub = M.empty

combine :: Sub -> Sub -> Sub
combine s1 s2 = M.union s1 (M.map (substitute s1) s2)

combineAll :: [Sub] -> Sub
combineAll = foldr combine noSub

remove :: TEnv -> String -> TEnv
remove (TEnv e) s = TEnv (M.delete s e)

insert :: TEnv -> String -> Scheme -> TEnv
insert (TEnv e) s t = TEnv (M.insert s t e)

generalise :: TEnv -> Type -> Scheme
generalise e t = Scheme vars t
  where vars = S.toList (S.difference (freeVars t) (freeVars e))

emptyScheme :: Type -> Scheme
emptyScheme t = Scheme (S.toList (freeVars t)) t

data InferState = InferState {
  isVars :: Int,
  isEnv  :: TEnv
}

type TI a = ExceptT String (State InferState) a

runTI :: TEnv -> TI a -> (Either String a, InferState)
runTI env = (`runState` initState) . runExceptT
  where initState = InferState { isVars = 0, isEnv = env }

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
unify (TC n1 ts1) (TC n2 ts2)
  | n1 == n2 
    = do subs <- zipWithM unify ts1 ts2
         return $ combineAll subs
  | otherwise = throwError $ "Cannot unify " ++ show n1 ++ " with " ++ show n2
unify t1 t2
  | t1 == t2  = return noSub
  | otherwise = throwError $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

inferSub :: TEnv -> Expr -> TI (Sub, Type)
inferSub (TEnv e) (Id v)
  = case M.lookup v e of
      Nothing     -> throwError $ "Unbound variable " ++ v
      Just scheme -> do t <- refreshVars scheme
                        return (noSub, t)
inferSub (TEnv e) (Cons v)
  = case M.lookup v e of
      Nothing     -> throwError $ "Constructor not defined " ++ v
      Just scheme -> do t <- refreshVars scheme
                        return (noSub, t)
inferSub _ (Number _)
  = return (noSub, TInt)
inferSub _ (Chars _)
  = return (noSub, TList TChar)
inferSub e (Lam l r)
  = do env <- foldM (\e'@(TEnv tenv) s ->
                -- make sure we don't have type constructor with the same name
                case M.lookup s tenv of
                  Nothing -> do
                    var <- nextVar
                    let TEnv e'' = remove e' s
                        env = TEnv $ M.union e'' (M.singleton s (Scheme [] var))
                    return env
                  _ -> return e'
              ) e (ids l)
       (sl, tl) <- inferSub env l
       (sr, tr) <- inferSub (substitute sl env) r
       return(combine sl sr, TFun (substitute sr tl) tr)
  where ids x = map fst (fromJust (match x x))
inferSub e (Data n ts cons)
  = do e' <- foldM updateEnv e ts
       mapM_ (inferSub e') (concatMap snd cons)
       ts' <- mapM (inferSub e' . Id) ts
       let ts'' = map snd ts'
       return (noSub, TC n ts'')
  where updateEnv env t
          = do var <- nextVar
               let TEnv e' = remove env t
                   env'    = TEnv $ M.union e' (M.singleton t (Scheme [] var))
               return env'
inferSub e (App l r)
  = do var      <- nextVar
       (sl, tl) <- inferSub e l
       (sr, tr) <- inferSub (substitute sl e) r
       s        <- unify (substitute sr tl) (TFun tr var)
       return (combineAll [s, sr, sl], substitute s var)
inferSub e (Let x expr1 expr2)
  = inferSub e (App (Lam x expr2) expr1)
inferSub e (Def x expr)
  = do var     <- nextVar
       s'@InferState{..} <- get
       let TEnv e'   = remove e x
           env       = TEnv $ M.union e' (M.singleton x (Scheme [] var))
       (s, t)  <- inferSub env expr
       put s'{  isEnv = insert isEnv x (generalise env t) }
       return (s, t)
inferSub _ (List [])
  = do var <- nextVar
       return (noSub, TList var)
inferSub e (List (e1 : es))
  = do (s1, t1)       <- inferSub e e1
       (ss, TList ts) <- inferSub (substitute s1 e) (List es)
       s              <- unify t1 ts
       return (ss, TList $ substitute s ts)
inferSub e (PAbs p)
  = do (s, TList t) <- inferSub e (List p)
       return (s, t)
inferSub e (Quot q) = inferSub e q

inferType :: Expr -> TI Type
inferType expr
  = do InferState{..} <- get
       (sub, t) <- inferSub isEnv expr
       return $ substitute sub t

runTypeInference :: TEnv -> Expr -> Either String Type
runTypeInference env expr
  = let (t, _) = runTI env $ inferType expr in t 
