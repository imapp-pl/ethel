module Compiler where

import qualified Data.HashMap.Strict as HM
import Control.Monad (foldM, liftM)

import CompilerState
import EVMCode
import Syntax
-- import qualified Stack

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
  compileDecls (decls prog)
  mainDecl <- compileMain (mainExpr prog)

  -- Compute code offsets for each declaration:
  -- First build a map of declaration to code offsets,
  -- then use it to complete JUMP instructions while mergins code.
  declOffsets <- foldM (\ dos@((d,o):_) decl ->
                         do Just info <- getDeclInfo d 
                            let size = codeLength (diCode info)
                            return $ (decl,o+size):dos )
                 [(mainDecl, 0)]
                 (decls prog)

  -- complete addresses and merge code
  let decl2offset = HM.fromList declOffsets
  completedCode <- foldM
                   (\ codeList decl ->
                     do Just info <- getDeclInfo decl
                        let code = diCode info
                            code' = map (rewriteJump decl2offset) code
                        return $ code' : codeList)
                   []
                   (mainDecl : (decls prog))
  
  return $ concat $ reverse completedCode

  where
    rewriteJump decl2offset (EXTFuncAddr decl) = EVMPush $ makeWord32 offset
      where (Just offset) = HM.lookup decl decl2offset
    rewriteJump _ instr = instr

compileMain :: Expression -> Compiler Declaration
compileMain exp = do
  code <- compileExpr exp
  -- store the result in memory and generate the return instruction
  let mainCode = code ++
                 [ EVMPush (makeWord 0), EVMSimple MSTORE
                 , EVMPush (makeWord 32), EVMPush (makeWord 0)
                 , EVMSimple RETURN ]
  let mainDecl = makeDecl (pos exp) "main" [] exp
  setDeclInfo mainDecl (DeclInfo mainCode)
  return mainDecl

compileDecls :: [Declaration] -> Compiler ()
compileDecls decls = do
  mapM_ insertDecl decls
  mapM_ compileDecl decls

  where insertDecl decl = insertOrReport (declIdent decl) decl

compileDecl :: Declaration -> Compiler ()
compileDecl decl = do
  global <- onTopLevel
  if declArgs decl == []  
    then if global then compilerError (declPos decl) $
                        "Global vars not supported yet"
         else compileLocalVarDecl decl
    else if not global then compilerError (declPos decl) $
                         "Local functions are not supported yet"
         else compileFuncDecl decl

compileLocalVarDecl :: Declaration -> Compiler ()
compileLocalVarDecl decl = error "Not implemented yet!"

compileFuncDecl :: Declaration -> Compiler ()
compileFuncDecl decl = do
  enterScope
  clearStack
  -- declare each arg as a local var
  argDecls <- mapM (declareArg (declPos decl)) (declArgs decl)
  -- allocate slots for args on the local stack
  mapM_ pushStack argDecls  
  bodyCode <- compileExpr (declBody decl)
  let code = [EXTComment $ "BEGIN func " ++ declIdent decl]
             ++ bodyCode
             -- move return value under the return address
             ++ [EVMSwap (1 + declArity decl)] 
             -- pop function args from the stack
             ++ replicate (length $ declArgs decl) (EVMSimple POP)
             -- return to caller
             ++ [ EVMSimple JUMP
                , EXTComment $ "END func " ++ declIdent decl]
  leaveScope
  -- update function info
  setDeclInfo decl (DeclInfo code)
  
  where declareArg pos arg = do
          let argDecl = makeArgDecl pos arg
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
                 else do
                   maybeOff <- stackOffset decl
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
  
                    
                    
               
