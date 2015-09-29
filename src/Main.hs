import Interpreter 

import Control.Applicative
import System.Environment

welcome :: IO ()
welcome = putStrLn "REPL 0.1"

loadUserCode :: IO (Maybe String)
loadUserCode = do
  args <- getArgs
  case args of
   (a : _) -> Just <$> readFile a
   _       -> return Nothing

main :: IO ()
main = do 
  corelib  <- readFile "corelib.code"
  usercode <- loadUserCode
  welcome
  runREPL corelib usercode
