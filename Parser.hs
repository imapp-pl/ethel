module Parser where

import Data.Functor.Identity

import Text.Parsec 
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as E

import Syntax hiding (inParens)

languageDef :: Lang.LanguageDef st
languageDef = Lang.emptyDef
              { Tok.commentStart = "/*"
              , Tok.commentEnd   = "*/"
              , Tok.commentLine  = "//"
              , Tok.nestedComments = True
              , Tok.identStart   = letter <|> oneOf "_'"
              , Tok.identLetter  = alphaNum <|> oneOf "_'"
              , Tok.opStart      = oneOf "+-*/=<>&|!:"
              , Tok.opLetter     = Tok.opStart languageDef
              , Tok.reservedNames = ["let", "in", "return"
                                    , "if", "then", "else"
                                    , "new"]
              , Tok.reservedOpNames = ["+", "-", "*", "/", "=",
                                       "==", "<>", "<", "<=", ">", ">=",
                                       "&&", "||", "!", 
                                       ":="]
              , Tok.caseSensitive = True
              }

lexer = Tok.makeTokenParser languageDef

type Parser a = ParsecT String () Identity a

keyword :: String -> Parser ()
keyword = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

operator :: String -> Parser ()
operator = Tok.reservedOp lexer

literal :: Parser Integer
literal = Tok.natural lexer

inParens :: Parser a -> Parser a
inParens = Tok.parens lexer

inBrackets :: Parser a -> Parser a
inBrackets = Tok.brackets lexer

program :: Parser Program
program = do
  Tok.whiteSpace lexer
  decls <- many declaration
  keyword "return"
  exp <- expression
  return $ Program decls exp

declaration :: Parser Declaration
declaration = do
  pos <- getPosition
  keyword "let"
  id <- identifier
  args <- many identifier
  operator "="
  body <- expression
  return $ makeDecl pos id args body

expression :: Parser Expression
expression = do
  exps <- (letExpression <|> ifExpression <|> numericExpression)
          `sepBy1` (operator ";")
  return $ foldr1 SeqExpr exps  		   

letExpression :: Parser Expression
letExpression = do
  decl <- declaration
  keyword "in"
  body <- expression
  return $ LetExpr decl body

ifExpression :: Parser Expression
ifExpression = do
  pos <- getPosition
  keyword "if"
  condExpr <- expression
  keyword "then"
  thenExpr <- expression
  keyword "else"
  elseExpr <- expression
  return $ IfExpr pos condExpr thenExpr elseExpr

numericExpression :: Parser Expression
numericExpression = E.buildExpressionParser table (primaryExpression False)
  where table = [ [ prefix "!" ] 
                  -- [ prefix "-", prefix "+" ]
                , [ binary "*", binary "/" ]
                , [ binary "+", binary "-" ]
                , [ binary "==", binary "<>",
                    binary "<", binary "<=",
                    binary ">", binary ">=" ]
		, [ assign ]
                ]
        prefix op = E.Prefix $ do { operator op;
                                    pos <- getPosition;
                                    return $ UnOpExpr pos op } 

        binary op = E.Infix ( do { operator op; return $ BinOpExpr op } )
                    E.AssocLeft

        assign = E.Infix ( do { operator ":="; return AssignExpr } )
                 E.AssocRight

primaryExpression :: Bool -> Parser Expression
primaryExpression isArg = 
    literalExpression <|> 
    inParens expression <|>    
    memIndexExpression <|>
    do pos <- getPosition
       ident <- identifier
       args <- if isArg then return [] else many (primaryExpression True)
       case args of 
         [] -> return $ VarExpr pos ident
         exps -> return $ CallExpr pos ident exps

literalExpression :: Parser Expression
literalExpression = do
  pos <- getPosition
  num <- literal
  return $ LitExpr pos num

memIndexExpression :: Parser Expression
memIndexExpression = do
  pos <- getPosition
  indexExp <- inBrackets expression
  return $ MemIndexExpr pos indexExp

parseString input =
  case runParser program () "source" input of
    Left err -> error $ show err
    Right p  -> p

parseFile path = do
  input <- readFile path
  let prog = parseString input
  putStrLn (show prog)
  
