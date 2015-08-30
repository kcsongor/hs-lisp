{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator(
  Evaluator(..),
  ProgramState(..),
  runEval,
  repl,
) where

import Language
import Types

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.IORef

data ProgramState = ProgramState {
  counter :: IORef Int
}

runEval :: Evaluator a -> ProgramState -> IO a
runEval = runReaderT . run

-- IO needed for REPL
newtype Evaluator a = Evaluator {
  run :: ReaderT ProgramState IO a 
} deriving ( 
    Applicative,
    Functor,
    Monad,
    MonadIO,
    MonadReader ProgramState
  )

--------------------------------------------------------------------------------

repl :: Evaluator ()
repl = do
  liftIO $ putStr ">> "
  line <- liftIO getLine
  when (line /= ":q") $ do 
    ProgramState{..} <- ask
    liftIO $ modifyIORef counter (+1)
    case parseString line of
      Just code -> liftIO $ print $ eval code
      Nothing   -> liftIO $ putStrLn "couldn't parse"
    repl

-- | TODO:
eval :: Expr -> Expr
eval (List (Id "+" : vs))
  = Number $ foldr1 (+) (map (\(Number n) -> n) (map eval vs))
eval (List (Id "-" : vs))
  = Number $ foldr1 (-) (map (\(Number n) -> n) (map eval vs))
------
eval (List ([Id "eval", t]))
  = case eval t of
      Quot c -> eval c
      s       -> s -- TODO: should be type error
eval (List l)
  = List (map eval l)
eval (Quot q)
  = Quot q
eval (Id n) -- everything is either a number or a string obv
  = case parseNum n of
      Just num -> num
      Nothing  -> Id n
