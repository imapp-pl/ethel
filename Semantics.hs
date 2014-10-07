module Semantics where

import Compiler
import Syntax

data DeclInfo = DeclInfo -- nothing yet

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
    

checkDecls :: [Declaration] -> CompilerMonad DeclInfo ()
checkDecls decls = do
  mapM_ insertDecl decls
  mapM_ checkDecl decls

  where insertDecl decl = insertOrReport (declIdent decl) decl

checkDecl :: Declaration -> CompilerMonad DeclInfo ()
checkDecl decl = do
  enterScope
  mapM_ (declareArg (declPos decl)) (declArgs decl)
  checkExpr (declBody decl)
  leaveScope

  where declareArg pos arg = do
          let argDecl = makeArgDecl pos arg
          insertOrReport arg argDecl

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

  
                    
                    
               
