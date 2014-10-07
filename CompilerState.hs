module CompilerState where

import Control.Monad.State 
import qualified Data.HashMap.Strict as HM
import qualified Syntax as S


type ErrorMsg = String

type SymbolTable = HM.HashMap S.Ident S.Declaration
type DeclTable info = HM.HashMap S.Declaration info

data CompilerState info = CompilerState
                          { csErrorMsgs :: [ErrorMsg]
                          , csScopes :: [SymbolTable]
                          , csDeclInfo :: DeclTable info
                          }

emptyState :: CompilerState info
emptyState = CompilerState
             { csErrorMsgs = []
             , csScopes = [HM.empty]
             , csDeclInfo = HM.empty
             }

type CompilerMonad info = State (CompilerState info)


enterScope :: CompilerMonad info ()
enterScope = do
  modify $ \ cs -> cs { csScopes = HM.empty : csScopes cs }

leaveScope :: CompilerMonad info ()
leaveScope = do
  modify $ \ cs -> cs { csScopes = tail (csScopes cs) }


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


reportError :: ErrorMsg -> CompilerMonad info ()
reportError msg =
  modify $ \ cs -> cs { csErrorMsgs = msg : csErrorMsgs cs } 

getErrors :: CompilerMonad info [ErrorMsg]
getErrors = gets csErrorMsgs

hasErrors :: CompilerMonad info Bool
hasErrors = do
  msgs <- gets csErrorMsgs
  return $ msgs /= []

