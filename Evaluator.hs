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

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.IORef

data ProgramState = ProgramState {
  counter :: IORef Int
}

repl :: Evaluator ()
repl = do
  liftIO $ putStr ">> "
  line <- liftIO getLine
  when (line /= ":q") $ do 
    ProgramState{..} <- ask
    liftIO $ modifyIORef counter (+1)
    liftIO $ print (parseString line)
    repl


runEval :: Evaluator a -> ProgramState -> IO a
runEval = runReaderT . run

-- IO needed for REPL
newtype Evaluator a = Evaluator {
  run :: ReaderT ProgramState IO a 
} deriving ( 
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadReader ProgramState
  )
