module Syntax where

import qualified Text.Parsec.Pos as Pos
import qualified Text.PrettyPrint as PP

type Ident = String
type Position = Pos.SourcePos

data Expression = LitExpr
                  { pos :: Position
                  , value :: Integer }

                | VarExpr
                  { pos :: Position
                  , name :: Ident }

                | CallExpr
                  { pos :: Position
                  , func :: Ident
                  , callArgs :: [Expression] }

                | LetExpr
                  { letDecl :: Declaration
                  , letBody :: Expression }

                | IfExpr
                  { pos :: Position
                  , condExpr :: Expression
                  , thenExpr :: Expression
                  , elseExpr :: Expression }

  		| SeqExpr
		  { fstExp :: Expression
		  , sndExp :: Expression }		  

                | UnOpExpr
                  { pos :: Position
                  , unaryOp :: Ident
                  , argExpr :: Expression }

                | BinOpExpr
                  { binaryOp :: Ident
                  , lhsExpr :: Expression
                  , rhsExpr :: Expression }

                | MemIndexExpr
                  { pos :: Position
                  , indexExpr :: Expression }

                | AssignExpr
                  { asgnLhs :: Expression
                  , asgnRhs :: Expression }


data DeclType = DefDecl | ArgDecl

data Declaration = Declaration
                   { declPos   :: Position
                   , declType  :: DeclType
                   , declIdent :: Ident
                   , declArgs  :: [Ident]
                   , declBody  :: Expression }

instance Eq Declaration where
    d1 == d2  = declPos d1 == declPos d2 
                && declIdent d1 == declIdent d2

declArity :: Declaration -> Int
declArity = length . declArgs


makeDecl pos ident args body = Declaration
                               { declPos = pos
                               , declType = DefDecl
                               , declIdent = ident
                               , declArgs = args
                               , declBody = body }

makeArgDecl pos ident = Declaration
                        { declPos = pos
                        , declType = ArgDecl
                        , declIdent = ident
                        , declArgs = []
                        , declBody = VarExpr pos ident }
                        -- body does not matter for args

fakeDecl = makeArgDecl (Pos.initialPos "") "$"


data Program = Program
               { decls :: [Declaration]
               , mainExpr :: Expression }


infixl 6 <+>
(<+>) = (PP.<+>)
(<>) = (PP.<>)

infixl 5 $$, $+$
($$) = (PP.$$)
($+$) = (PP.$+$)

programToDoc prog =
  (PP.vcat $ map (\d -> declToDoc d $$ PP.text "") (decls prog))
  $+$
  PP.text "return" <+> exprToDoc False (mainExpr prog)
                    

declToDoc decl =
  let head = PP.text "let" <+> PP.text (declIdent decl)
             <+> PP.hsep (map  PP.text (declArgs decl))
             <+> PP.text "="
  in
   case (declBody decl) of
     e@(LetExpr _ _) -> head $$ PP.nest 4 (exprToDoc False e)
     e -> head <+> exprToDoc False e

exprToDoc _ (LitExpr _ v) = PP.text $ show v

exprToDoc _ (VarExpr _ id) = PP.text id

exprToDoc isArg(CallExpr _ func args) =
    inParens isArg $
    PP.text func <+> PP.hsep (map (exprToDoc True) args)

exprToDoc isArg (LetExpr decl body) =
    inParens isArg $
    declToDoc decl $+$ 
    PP.text "in" <+> 
    exprToDoc False body

exprToDoc isArg (IfExpr _ cond texp fexp) =
    inParens isArg $
    PP.text "if" <+> exprToDoc False cond <+> 
    PP.text "then" <+> exprToDoc False texp $$
    PP.text "else" <+> exprToDoc False fexp

exprToDoc isArg (SeqExpr exp1 exp2) = 
    inParens isArg $
    exprToDoc False exp1 <+> 
    PP.text ";" $+$
    exprToDoc False exp2

exprToDoc isArg (UnOpExpr _ op arg) =
    inParens isArg $
    PP.text op <+> exprToDoc True arg

exprToDoc isArg (BinOpExpr op lhs rhs) =
    inParens isArg $
    exprToDoc True lhs <+>
    PP.text op <+>
    exprToDoc True rhs

exprToDoc isArg (MemIndexExpr _ indexExp) =
    PP.text "[" <> exprToDoc False indexExp <> PP.text "]"

exprToDoc isArg (AssignExpr lhsExp rhsExp) =
    inParens isArg $
    exprToDoc True lhsExp <+>
    PP.text ":=" <+>
    exprToDoc True rhsExp 

inParens True doc = PP.text "(" <+> doc <+> PP.text ")"
inParens False doc = doc

instance Show Program where
  show = PP.render . programToDoc

instance Show Declaration where
  show = PP.render . declToDoc
  
instance Show Expression where
  show = PP.render . exprToDoc False
