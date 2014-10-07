module Syntax where

import qualified Text.Parsec.Pos
import qualified Text.PrettyPrint as PP

type Ident = String
type Position = Text.Parsec.Pos.SourcePos

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

                | DefExpr
                  { localDecls :: [Declaration]
                  , defBody :: Expression }

                | IfExpr
                  { pos :: Position
                  , condExpr :: Expression
                  , thenExpr :: Expression
                  , elseExpr :: Expression }

                | UnOpExpr
                  { pos :: Position
                  , unaryOp :: Ident
                  , argExpr :: Expression }

                | BinOpExpr
                  { binaryOp :: Ident
                  , lhsExpr :: Expression
                  , rhsExpr :: Expression }


data DeclType = DefDecl | ArgDecl

data Declaration = Declaration
                   { declPos   :: Position
                   , declType  :: DeclType
                   , declIdent :: Ident
                   , declArgs  :: [Ident]
                   , declBody  :: Expression }

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

newtype Program = Program { decls :: [Declaration] }


infixl 6 <+>
(<+>) = (PP.<+>)

infixl 5 $$, $+$
($$) = (PP.$$)
($+$) = (PP.$+$)

programToDoc prog = PP.vcat $
                    map (\d -> declToDoc d $$ PP.text "") (decls prog)
                    

declToDoc decl =
  let head = PP.text "def" <+> PP.text (declIdent decl)
             <+> PP.hsep (map  PP.text (declArgs decl))
             <+> PP.text "="
  in
   case (declBody decl) of
     e@(DefExpr _ _) -> head $$ PP.nest 4 (exprToDoc False e)
     e -> head <+> exprToDoc False e

exprToDoc _ (LitExpr _ v) = PP.text $ show v
exprToDoc _ (VarExpr _ id) = PP.text id
exprToDoc isArg(CallExpr _ func args) =
  inParens isArg $
  PP.text func <+> PP.hsep (map (exprToDoc True) args)
exprToDoc isArg (DefExpr decls body) =
  inParens isArg $
  PP.vcat (map declToDoc decls)
  $+$ PP.text "in" <+> exprToDoc False body
exprToDoc isArg (IfExpr _ cond texp fexp) =
  inParens isArg $
  PP.text "if" <+> exprToDoc False cond <+> 
  PP.text "then" <+> exprToDoc False texp $$
  PP.text "else" <+> exprToDoc False fexp
exprToDoc isArg (UnOpExpr _ op arg) =
  inParens isArg $
  PP.text op <+> exprToDoc True arg
exprToDoc isArg (BinOpExpr op lhs rhs) =
  inParens isArg $
  exprToDoc True lhs <+>
  PP.text op <+>
  exprToDoc True rhs

inParens True doc = PP.text "(" <+> doc <+> PP.text ")"
inParens False doc = doc

instance Show Program where
  show = PP.render . programToDoc

instance Show Declaration where
  show = PP.render . declToDoc
  
instance Show Expression where
  show = PP.render . exprToDoc False
