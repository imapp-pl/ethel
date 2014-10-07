module Main where

import Control.Monad.State 

import Syntax
import Parser
import Semantics
import Compiler

compile :: String -> IO ()
compile path = do
  input <- readFile path
  let prog = parseString input
  putStrLn (show prog)

  let semAnalysis = do { checkDecls (decls prog) } 
  let (_,endState) = runState semAnalysis emptyState
  let global = head (csScopes endState)

  putStrLn "Global scope:"
  putStrLn (show global)

  putStrLn "Error messages:"
  mapM_ putStrLn (csErrorMsgs endState)
  
  
