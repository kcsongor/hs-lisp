{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Evaluator(
  Evaluator(..),
  ProgramState(..),
  repl,
  runEval,
) where

import Language

import Control.Monad.State
import Control.Applicative

data ProgramState = ProgramState {
  counter :: Int
}

repl :: Evaluator ()
repl = do
  liftIO $ putStr ">> "
  line <- liftIO getLine
  when (line /= ":q") $ do
    liftIO $ print $ parseString line
    repl

runEval :: Evaluator a -> ProgramState -> IO (a, ProgramState)
runEval = runStateT . run

-- IO needed for REPL
newtype Evaluator a = Evaluator { run :: StateT ProgramState IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadState ProgramState )
