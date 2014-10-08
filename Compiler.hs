module Compiler where

import qualified Data.HashMap.Strict as HM
import Data.List (mapAccumL)
import Control.Monad (foldM, liftM)

import CompilerState
import EVMCode
import Syntax

type Code = EVMCode Declaration

data DeclInfo = DeclInfo 
                { diCode :: Code }

instance Show DeclInfo where
  show = show . diCode 

type Compiler = CompilerMonad DeclInfo

compilerError :: Position -> String -> Compiler ()
compilerError pos msg =
  reportError (show pos ++ ": Error: " ++ msg)
  

insertOrReport :: Ident -> Declaration -> Compiler ()
insertOrReport ident decl = do
  let ident = declIdent decl
  maybeDecl <- lookupCurrent ident
  case maybeDecl of
    Just decl' -> compilerError (declPos decl) $
                  "Duplicate declaration of '" ++ ident ++ 
                  "', previous declaration at " ++ show (declPos decl')
    Nothing -> symtabInsert ident decl

    

compileProgram :: Program -> Compiler Code
compileProgram prog = do
  -- enter top-level
  enterScope 
  
  -- compile top-level declarations and the main expression
  compiledDecls <- compileDecls (decls prog)
  -- (mainDecl, mainCode) <- compileMain (mainExpr prog)
  compiledMain <- compileMain (mainExpr prog)              

  -- Compute code offsets for each declaration:
  -- first build a map of declaration to code offsets,
  -- then use it to complete JUMP instructions.
  let (_,declOffsets) = mapAccumL (\ offset (decl, code) -> 
                                       let codeSize = codeLength code 
                                       in (offset + codeSize, (decl, offset)))
                        0 
                        (compiledMain : compiledDecls)

  -- complete addresses and merge code
  let decl2offset = HM.fromList declOffsets
  let rewrittenCode = map (\ (decl,code) -> map (rewriteJump decl2offset) code)
                      (compiledMain : compiledDecls)

  return $ concat rewrittenCode

  where
    rewriteJump decl2offset (EXTFuncAddr decl) = EVMPush $ makeWord32 offset
        where (Just offset) = HM.lookup decl decl2offset
    rewriteJump _ instr = instr

compileMain :: Expression -> Compiler (Declaration, Code)
compileMain exp = do
  code <- compileExpr exp
  -- store the result in memory and generate the return instruction
  let mainCode = code ++
                 [ EVMPush (makeWord 0), EVMSimple MSTORE
                 , EVMPush (makeWord 32), EVMPush (makeWord 0)
                 , EVMSimple RETURN ]
  let mainDecl = makeDecl (pos exp) "main" [] exp
  return (mainDecl, mainCode)

compileDecls :: [Declaration] -> Compiler [(Declaration, Code)]
compileDecls decls = do
  mapM_ insertDecl decls
  codes <- mapM compileDecl decls
  return $ zip decls codes

  where insertDecl decl = insertOrReport (declIdent decl) decl

compileDecl :: Declaration -> Compiler Code
compileDecl decl = do
  global <- onTopLevel
  if declArgs decl == []  
  then if global 
       then do compilerError (declPos decl) $
                   "Global vars not supported yet"
               return []
       else compileLocalVarDecl decl
  else if not global 
       then do compilerError (declPos decl) $
                   "Local functions are not supported yet"
               return []
       else compileFuncDecl decl

compileLocalVarDecl :: Declaration -> Compiler Code
compileLocalVarDecl decl = error "Not implemented yet!"


compileFuncDecl :: Declaration -> Compiler Code
compileFuncDecl decl = do
  enterScope
  clearStack
  -- declare each arg as a local var
  argDecls <- mapM (declareArg (declPos decl)) (declArgs decl)
  -- allocate slots for args on the local stack
  mapM_ pushStack argDecls  
  bodyCode <- compileExpr (declBody decl)
  stackSz <- stackSize
  let code = [EXTComment $ "BEGIN func " ++ declIdent decl]
             ++ bodyCode
             -- move return value under the return address
             -- ++ [EVMSwap (1 + declArity decl)] 
             ++ [EVMSwap (stackSz + 1)]
             -- pop function args from the stack
             -- ++ replicate (length $ declArgs decl) (EVMSimple POP)
             ++ replicate (stackSz - 1) (EVMSimple POP)
             -- return to caller
             ++ [ EVMSimple JUMP
                , EXTComment $ "END func " ++ declIdent decl]
  leaveScope
  return code
  
    where declareArg pos arg = 
              do let argDecl = makeArgDecl pos arg
                 insertOrReport arg argDecl
                 return argDecl

compileExpr :: Expression -> Compiler Code 

compileExpr (LitExpr pos lit) = do
  allocStackItem
  return [EVMPush (makeWord lit)]

compileExpr (VarExpr pos ident) = do
  maybeDecl <- lookupSymbol ident
  case maybeDecl of
    Nothing -> do compilerError pos $ "Undefined identifier '" ++ ident ++ "'"
                  return []
    Just decl -> if length (declArgs decl) > 0 
                 then do compilerError pos $
                           "Symbol '" ++ ident ++ "' requires " ++
                           show (length (declArgs decl)) ++ " argument(s)"
                         return []
                 else do maybeOff <- stackOffset decl
                         case maybeOff of
                           Nothing -> error $ "Global vars not supported yet"
                           Just off -> do pushStack decl
                                          return [EVMDup (off+1)]

compileExpr (CallExpr pos ident args) = do
  maybeDecl <- lookupSymbol ident
  case maybeDecl of
    Nothing -> do compilerError pos $ "Undefined identifier '" ++ ident ++ "'"
                  return []
    Just decl ->
      do let arity = length (declArgs decl)
         if arity /= length args
         then do compilerError pos $
                   "Function '" ++ ident ++ "' requires " ++
                   show arity ++ " arguments"
                 return []
                
         else do allocStackItem -- leave space for return address
                 callCode <- compileCall decl args
                 let callLength = codeLength callCode
                 let lengthWord = makeWord (callLength + 2)
                 return $
                   [ EXTComment $ "call to " ++ declIdent decl,
                     EVMPush lengthWord,
                     EVMSimple PC,
                     EVMSimple ADD,
                     EXTComment "call args" ] ++ callCode
                   
  where compileCall :: Declaration -> [Expression] -> Compiler Code
        compileCall decl args = do
          argCodes <- mapM compileExpr args
          let coda = [ EXTFuncAddr decl, EVMSimple JUMP, EVMSimple JUMPDEST,
                       EXTComment "end of call sequence" ]
          return $ foldr (++) coda argCodes

compileExpr (LetExpr decl body) = do
  insertOrReport (declIdent decl) decl
  declCode <- compileExpr (declBody decl)
  -- allocate place on the stack for the local var
  popStack
  pushStack decl
  bodyCode <- compileExpr body
  return $ declCode ++ bodyCode
          
compileExpr (BinOpExpr op lhs rhs) = do
  lhsCode <- compileExpr lhs
  rhsCode <- compileExpr rhs
  popStack
  popStack
  allocStackItem
  return $ lhsCode ++ rhsCode ++ [EVMSimple ADD]

                      
  
{-
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
  
                    
                    
               
