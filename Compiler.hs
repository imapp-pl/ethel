module Compiler where

import CompilerState
import Syntax
import EVMCode
import qualified Stack

type Code = EVMCode Declaration

data DeclInfo = DeclInfo 
                { diCode :: Code }

instance Show DeclInfo where
  show = show . diCode 

compileError :: Position -> String -> CompilerMonad DeclInfo ()
compileError pos msg =
  reportError (show pos ++ ": Error: " ++ msg)
  

insertOrReport :: Ident -> Declaration -> CompilerMonad DeclInfo ()
insertOrReport ident decl = do
  let ident = declIdent decl
  maybeDecl <- lookupCurrent ident
  case maybeDecl of
    Just decl' -> compileError (declPos decl) $
                  "Duplicate declaration of '" ++ ident ++ 
                  "', previous declaration at " ++ show (declPos decl')
    Nothing -> symtabInsert ident decl
    

compileProgram :: Program -> CompilerMonad DeclInfo ()
compileProgram prog = do
  -- enterScope
  compileDecls (decls prog)
  -- leaveScope

compileDecls :: [Declaration] -> CompilerMonad DeclInfo ()
compileDecls decls = do
  mapM_ insertDecl decls
  mapM_ compileDecl decls

  where insertDecl decl = insertOrReport (declIdent decl) decl

compileDecl :: Declaration -> CompilerMonad DeclInfo ()
compileDecl decl = do
  global <- onTopLevel
  if declArgs decl == []  
    then if global then compileError (declPos decl) $
                        "Global vars not supported yet"
         else compileLocalVarDecl decl
    else if not global then compileError (declPos decl) $
                         "Local functions are not supported yet"
         else compileFuncDecl decl

compileLocalVarDecl :: Declaration -> CompilerMonad DeclInfo ()
compileLocalVarDecl decl = undefined

compileFuncDecl :: Declaration -> CompilerMonad DeclInfo ()
compileFuncDecl decl = do
  enterScope
  clearStack
  -- declare each arg as a local var
  argDecls <- mapM (declareArg (declPos decl)) (declArgs decl)
  -- allocate slots for args on the local stack
  mapM_ pushStack argDecls  
  code <- compileExpr (declBody decl)
  -- TODO: code to pop args and return
  leaveScope
  -- update function info
  setDeclInfo decl (DeclInfo code)
  
  where declareArg pos arg = do
          let argDecl = makeArgDecl pos arg
          insertOrReport arg argDecl
          return argDecl

compileExpr :: Expression -> CompilerMonad DeclInfo Code 

compileExpr (LitExpr pos lit) = do
  pushStack fakeDecl
  return [EVMPush (makeWord lit)]

compileExpr (VarExpr pos ident) = do
  maybeDecl <- lookupSymbol ident
  case maybeDecl of
    Nothing -> do compileError pos $ "Undefined identifier '" ++ ident ++ "'"
                  return []
    Just decl -> if length (declArgs decl) > 0 
                 then do compileError pos $
                           "Symbol '" ++ ident ++ "' requires " ++
                           show (length (declArgs decl)) ++ " argument(s)"
                         return []
                 else do
                   maybeOff <- stackOffset decl
                   case maybeOff of
                     Nothing -> error $ "Global vars not supported yet"
                     Just off -> do pushStack decl
                                    return [EVMDup (off+1)]

compileExpr (BinOpExpr op lhs rhs) = do
  lhsCode <- compileExpr lhs
  rhsCode <- compileExpr rhs
  popStack
  popStack
  pushStack fakeDecl
  return $ lhsCode ++ rhsCode ++ [EVMSimple ADD]

                      
  
{-
checkExpr :: Expression -> CompilerMonad DeclInfo ()

checkExpr (LitExpr _ _) = return ()

checkExpr (VarExpr pos ident) = do
  maybeDecl <- lookupSymbol ident
  case maybeDecl of
    Nothing -> compileError pos $ "Undefined identifier '" ++ ident ++ "'"
    Just decl -> if length (declArgs decl) > 0 
                 then compileError pos $
                      "Symbol '" ++ ident ++ "' requires " ++
                      show (length (declArgs decl)) ++ " argument(s)"
                 else return ()

checkExpr (CallExpr pos ident args) = do
  maybeDecl <- lookupSymbol ident
  case maybeDecl of
    Nothing -> compileError pos $ "Undefined identifier '" ++ ident ++ "'"
    Just decl -> do let arity = length (declArgs decl)
                    if arity /= length args
                      then compileError pos $
                           "Function '" ++ ident ++ "' requires " ++
                           show arity ++ " arguments"
                      else return ()
                    mapM_ checkExpr args

checkExpr (DefExpr decls body) = do
  enterScope
  checkDecls decls
  checkExpr body
  leaveScope

checkExpr (IfExpr pos cexp texp fexp) = do
  checkExpr cexp
  checkExpr texp
  checkExpr fexp

checkExpr (UnOpExpr pos op arg) = do
  checkExpr arg

checkExpr (BinOpExpr op lhs rhs) = do
  checkExpr lhs
  checkExpr rhs
-}
  
                    
                    
               
