module Main where

import Control.Monad.State 

import Syntax
import Parser
import CompilerState
import Compiler

compile :: String -> IO ()
compile path = do
  input <- readFile path
  let prog = parseString input
  putStrLn (show prog)

  let (_,endState) = runState (enterScope >> compileProgram prog) emptyState
  let global = head (csScopes endState)

  putStrLn "Global scope:"
  putStrLn (show global)

  putStrLn "Error messages:"
  mapM_ putStrLn (csErrorMsgs endState)

  putStrLn "Decls code:"
  putStrLn (show $ csDeclInfo endState)
  
