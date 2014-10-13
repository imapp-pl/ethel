module Compiler where

import Prelude hiding (EQ, LT, GT)
import qualified Data.HashMap.Strict as HM
import Data.List (mapAccumL)
import Control.Monad (foldM, liftM)

import CompilerState
import EVMCode
import Syntax


data DeclInfo = DeclInfo 
                { diCode :: EVMCode }

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

    

compileProgram :: Program -> Compiler (Int, EVMCode)
compileProgram prog = do
  -- enter top-level
  enterScope 
  
  -- compile top-level declarations and the main expression
  compiledDecls <- compileDecls (decls prog)
  -- (mainDecl, mainCode) <- compileMain (mainExpr prog)
  compiledMain <- compileMain (mainExpr prog)              

  let allCode = foldr (++) [] (map snd (compiledMain : compiledDecls))
                
      labelSize = computeLabelSize allCode

      (_, posCode) = mapAccumL (\ pos instr -> 
                                    let sz = instrSize labelSize instr 
                                    in  (pos + sz, (pos, instr)))
                     0 allCode

      posLabels = map (\ (pos, EXTLabel l) -> (l, pos) )
                      $ filter isLabel posCode

      label2pos = HM.fromList posLabels

      completedCode = map (replaceJumpLabel labelSize label2pos) allCode 

  return (labelSize, completedCode)

  where isLabel (_, EXTLabel _) = True
        isLabel _ = False

        replaceJumpLabel labelSize label2pos (EXTLabelAddr l) = 
            EVMPush $ makeNWord labelSize pos
            where --(Just pos) = HM.lookup l label2pos 
              pos = case HM.lookup l label2pos of
                      (Just p) -> p
                      Nothing -> error $ "cannot resolve label " ++ l
        replaceJumpLabel _ _ instr = instr

computeLabelSize :: EVMCode -> Int
computeLabelSize code = labelSize 1
    where evmCodeSize = codeSize 0 code
          labelAddrCnt = length $ filter isLabelAddrInstr code
          
          labelSize n = if evmCodeSize + (labelAddrCnt * n) < 2^(n * 8)
                        then n else labelSize (n+1)

          isLabelAddrInstr (EXTLabelAddr _) = True
          isLabelAddrInstr _ = False

compileMain :: Expression -> Compiler (Declaration, EVMCode)
compileMain exp = do
  code <- compileExpr exp
  -- store the result in memory and generate the return instruction
  let mainCode = code ++
                 [ EVMPush (makeWord 0), EVMSimple MSTORE
                 , EVMPush (makeWord 32), EVMPush (makeWord 0)
                 , EVMSimple RETURN ]
  let mainDecl = makeDecl (pos exp) "main" [] exp
  return (mainDecl, mainCode)

compileDecls :: [Declaration] -> Compiler [(Declaration, EVMCode)]
compileDecls decls = do
  mapM_ insertDecl decls
  codes <- mapM compileDecl decls
  return $ zip decls codes

  where insertDecl decl = insertOrReport (declIdent decl) decl

compileDecl :: Declaration -> Compiler EVMCode
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

compileLocalVarDecl :: Declaration -> Compiler EVMCode
compileLocalVarDecl decl = error "Not implemented yet!"


compileFuncDecl :: Declaration -> Compiler EVMCode
compileFuncDecl decl = do
  enterScope
  clearStack
  -- declare each arg as a local var
  argDecls <- mapM (declareArg (declPos decl)) (declArgs decl)
  -- allocate slots for args on the local stack
  mapM_ pushStack argDecls  
  bodyCode <- compileExpr (declBody decl)
  stackSz <- stackSize
  let funcLabel = "func." ++ declIdent decl
  let code = [ EXTComment $ "BEGIN func " ++ declIdent decl
             , EXTLabel $ funcLabel ]
             ++ bodyCode
             -- move return value under the return address
             ++ [EVMSwap stackSz]
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

compileExpr :: Expression -> Compiler EVMCode 

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
                 argsCode <- mapM compileExpr args
                 retLabel <- uniqueName "return" 
                 let funcLabel = "func." ++ declIdent decl
                 return $
                   [ EXTComment $ "call to " ++ declIdent decl,
                     EXTLabelAddr retLabel ]
                   ++ concat argsCode ++
                   [ EXTLabelAddr funcLabel, 
                     EVMSimple JUMP,
                     EVMSimple JUMPDEST,
                     EXTLabel retLabel,
                     EXTComment "end of call sequence" ]
                   
compileExpr (LetExpr decl body) = do
  insertOrReport (declIdent decl) decl
  declCode <- compileExpr (declBody decl)
  -- allocate place on the stack for the local var
  popStack
  pushStack decl
  bodyCode <- compileExpr body
  return $ declCode ++ bodyCode

compileExpr (IfExpr pos cexp texp fexp) = do
  condCode <- compileExpr cexp
  popStack
  thenCode <- compileExpr texp
  popStack
  elseCode <- compileExpr fexp
  thenLabel <- uniqueName "then"
  fiLabel <- uniqueName "fi"
  return $ condCode ++
             [EXTLabelAddr thenLabel, EVMSimple JUMPI] ++
             elseCode ++
             [EXTLabelAddr fiLabel, EVMSimple JUMP, EXTLabel thenLabel] ++
             thenCode ++
             [EXTLabel fiLabel]
          
compileExpr (BinOpExpr op lhs rhs) = do
  lhsCode <- compileExpr lhs
  rhsCode <- compileExpr rhs
  popStack
  popStack
  allocStackItem
  return $ lhsCode ++ rhsCode ++ map EVMSimple (opcode op)

      where opcode "+" = [ADD]
            opcode "-" = [SUB]                  
            opcode "*" = [MUL]
            opcode "/" = [DIV]
            opcode "%" = [MOD]
            opcode "==" = [EQ]
            opcode "<>" = [EQ,NOT]
            opcode "<"  = [LT]
            opcode "<=" = [GT,NOT]
            opcode ">"  = [GT]
            opcode ">=" = [LT,NOT]

compileExpr (UnOpExpr _ op arg) = do 
  argCode <- compileExpr arg
  popStack
  allocStackItem
  return $ argCode ++ map EVMSimple (opcode op)

      where opcode "+" = []
            opcode "-" = [NEG]
            opcode "!" = [NOT]
  
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
  
                    
                    
               
