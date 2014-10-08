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

  let (code,endState) = runState (compileProgram prog) emptyState
  let global = head (csScopes endState)

  putStrLn "Global scope:"
  putStrLn (show global)

  putStrLn "Error messages:"
  mapM_ putStrLn (csErrorMsgs endState)

  putStrLn "Generated code:"
  putStrLn $ show code

  
