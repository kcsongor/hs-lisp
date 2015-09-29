import Interpreter 

import Control.Applicative
import System.Environment
import System.IO

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
  hSetBuffering stdout NoBuffering
  corelib  <- readFile "corelib.code"
  usercode <- loadUserCode
  welcome
  runREPL corelib usercode
