module CompilerState where

import Control.Monad.State 
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as HM
import qualified Syntax as S
import qualified Stack
import qualified Text.Parsec.Pos as Pos

type ErrorMsg = String

type SymbolTable = HM.HashMap S.Ident S.Declaration
type DeclTable info = HM.HashMap S.Declaration info

data CompilerState info =
  CompilerState
  { csErrorMsgs :: [ErrorMsg]  
  , csScopes :: [SymbolTable]                 -- stack of scopes
  , csLocalStack :: Stack.Stack S.Declaration -- tracks local stack in functions
  }
  
instance H.Hashable Pos.SourcePos where
    hashWithSalt salt pos = 
        H.hashWithSalt salt (Pos.sourceName pos, 
                                    (Pos.sourceLine pos, Pos.sourceColumn pos))

instance H.Hashable S.Declaration where
    hashWithSalt salt decl = 
        H.hashWithSalt salt (S.declPos decl, S.declIdent decl)
    

emptyState :: CompilerState info
emptyState = CompilerState
             { csErrorMsgs = []
             , csScopes = [] 
             , csLocalStack = Stack.empty  
             }

type CompilerMonad info = State (CompilerState info)

enterScope :: CompilerMonad info ()
enterScope = do
  modify $ \ cs -> cs { csScopes = HM.empty : csScopes cs }

leaveScope :: CompilerMonad info ()
leaveScope = do
  modify $ \ cs -> cs { csScopes = tail (csScopes cs) }

onTopLevel :: CompilerMonad info Bool
onTopLevel = do
  scopes <- gets csScopes
  return $ 1 >= length scopes

-- Symbol lookup in current scope
lookupCurrent :: S.Ident -> CompilerMonad info (Maybe S.Declaration)
lookupCurrent ident = do
  (s : _) <- gets csScopes
  return $ HM.lookup ident s

-- Symbol lookup in all enclosing scopes
lookupSymbol :: S.Ident -> CompilerMonad info (Maybe S.Declaration)
lookupSymbol ident = do
  scopes <- gets csScopes
  case dropWhile notFound (map (HM.lookup ident) scopes) of
    [] -> return Nothing
    res:_ -> return res       
  where notFound Nothing = True
        notFound (Just _) = False

symtabInsert :: S.Ident -> S.Declaration -> CompilerMonad info ()
symtabInsert ident decl = do
  (s : scopes) <- gets csScopes
  modify $ \ cs -> let s' = HM.insert ident decl s
                   in  cs { csScopes = s' : scopes }

-- LOCAL STACK ----------------------------------------------------------

clearStack :: CompilerMonad info ()
clearStack = modify $ \ cs -> cs { csLocalStack = Stack.empty }

pushStack :: S.Declaration -> CompilerMonad info ()
pushStack decl = 
    modify $ \ cs -> cs { csLocalStack = Stack.push decl (csLocalStack cs) }

popStack :: CompilerMonad info ()
popStack = 
    modify $ \ cs -> cs { csLocalStack = Stack.pop (csLocalStack cs) }

allocStackItem :: CompilerMonad info ()
allocStackItem = pushStack S.fakeDecl

stackSize :: CompilerMonad info Int
stackSize = gets csLocalStack >>= return . Stack.stackSize

stackOffset :: S.Declaration -> CompilerMonad info (Maybe Stack.Offset)
stackOffset decl = gets csLocalStack >>= return . Stack.offset decl


-- ERROR MSGS ------------------------------------------------------------

reportError :: ErrorMsg -> CompilerMonad info ()
reportError msg =
  modify $ \ cs -> cs { csErrorMsgs = msg : csErrorMsgs cs } 

getErrors :: CompilerMonad info [ErrorMsg]
getErrors = gets csErrorMsgs

hasErrors :: CompilerMonad info Bool
hasErrors = do
  msgs <- gets csErrorMsgs
  return $ msgs /= []

