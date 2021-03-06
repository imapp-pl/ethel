module Compiler where

import Prelude hiding (EQ, LT, GT)
import qualified Data.HashMap.Strict as HM
import Data.List (mapAccumL)
import qualified Data.Set as Set
import Control.Monad (foldM, liftM, when)

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
  compiledMain <- compileMain (mainExpr prog)              

  let code   = foldr (++) [] (map snd (compiledMain : compiledDecls))
  let code'  = removeUnusedLabels code
  let code'' = rewriteLabels code'
  return code''

removeUnusedLabels :: EVMCode -> EVMCode
removeUnusedLabels code = filter isUsedLabel code
    where 
      isUsedLabel (EXTLabel l) = Set.member l labels
      isUsedLabel _ = True
                      
      labels = labels' Set.empty code

      labels' ll ((EXTLabelAddr l):code) = labels' (Set.insert l ll) code
      labels' ll (_:code) = labels' ll code
      labels' ll [] = ll

rewriteLabels :: EVMCode -> (Int, EVMCode)
rewriteLabels code = (labelSize, code')

    where labelSize = computeLabelSize code

          code' = map (rewriteLabel labelSize label2pos) code
        
          rewriteLabel labelSize label2pos (EXTLabelAddr l) = 
            case HM.lookup l label2pos of
              (Just pos) -> EVMPush $ makeNWord labelSize pos
              Nothing -> error $ "cannot resolve label " ++ l
          rewriteLabel _ _ instr = instr

          label2pos = HM.fromList posLabels

          posLabels = map (\ (pos, EXTLabel l) -> (l, pos) )
                      $ filter isLabel posCode

          (_, posCode) = mapAccumL (\ pos instr -> 
                                     let sz = instrSize labelSize instr 
                                     in  (pos + sz, (pos, instr)))
                         0 code

          isLabel (_, EXTLabel _) = True
          isLabel _ = False   

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
             -- pop function args from the stack
             ++ concat (replicate (stackSz - 1) [EVMSwap 1, EVMSimple POP])
             -- move return value under the return address
             ++ [EVMSwap 1]
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
                
         else do initSize <- stackSize

                 allocStackItem -- leave space for return address
                 checkStackSize (initSize + 1) ""

                 argsCode <- mapM compileExpr args
                 checkStackSize (initSize + 1 + length args) "call args"

                 retLabel <- uniqueName "return" 
                 let funcLabel = "func." ++ declIdent decl
                 -- popStack       -- the callee pops the return address 
                 popMany (length args)
                 checkStackSize (initSize + 1) "after call"

                 return $
                   [ EXTComment $ "call to " ++ declIdent decl,
                     EXTLabelAddr retLabel ]
                   ++ concat argsCode ++
                   [ EXTLabelAddr funcLabel, 
                     EVMSimple JUMP,
                     EXTLabel retLabel,
                     EXTComment "end of call sequence" ]
                   
compileExpr (LetExpr decl body) = do
  insertOrReport (declIdent decl) decl
  declCode <- compileExpr (declBody decl)
  -- allocate place on the stack for the local var
  popStack
  pushStack decl
  bodyCode <- compileExpr body
  popStack -- remove the local var after the body completes
  return $ declCode ++ bodyCode ++ [ EVMSwap 1, EVMSimple POP ]

compileExpr (IfExpr pos cexp texp fexp) = do
  initSize <- stackSize

  condCode <- compileExpr cexp
  popStack
  checkStackSize initSize "cond code"
  
  elseCode <- compileExpr fexp
  popStack
  checkStackSize initSize "else code"
  
  thenCode <- compileExpr texp
  checkStackSize (initSize+1) "then code"

  thenLabel <- uniqueName "then"
  fiLabel <- uniqueName "fi"
  return $ condCode ++
             [EXTLabelAddr thenLabel, EVMSimple JUMPI] ++
             elseCode ++
             [EXTLabelAddr fiLabel, EVMSimple JUMP, EXTLabel thenLabel] ++
             thenCode ++
             [EXTLabel fiLabel]
          
compileExpr (SeqExpr exp1 exp2) = do
  exp1Code <- compileExpr exp1
  popStack
  exp2Code <- compileExpr exp2
  return $
    exp1Code ++
    [ EVMSimple POP ] ++
    exp2Code

compileExpr (BinOpExpr op lhs rhs) = do
  rhsCode <- compileExpr rhs
  lhsCode <- compileExpr lhs
  popStack
  popStack
  allocStackItem
  return $ rhsCode ++ lhsCode ++ map EVMSimple (opcode op)

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

      where opcode "!" = [NOT]

compileExpr (MemIndexExpr _ indexExpr) = do
  indexCode <- compileExpr indexExpr
  return $ 
    indexCode ++ 
    [ EVMPush (makeWord 32), 
      EVMSimple MUL, 
      EVMSimple MLOAD ] 

compileExpr (AssignExpr (MemIndexExpr _ indexExpr) rhsExpr) = do
  rhsCode <- compileExpr rhsExpr
  allocStackItem
  indexCode <- compileExpr indexExpr
  popStack
  popStack
  return $
    rhsCode ++ 
    [ EVMDup 1 ] ++ 
    indexCode ++ 
    [ EVMPush (makeWord 32),
      EVMSimple MUL,
      EVMSimple MSTORE ]

compileExpr (AssignExpr lhsExpr rhsExpr) = do
  compilerError (pos lhsExpr) $
    "Syntax error: missing brackets '[ ]' on the LHS of assignment"
  return []

