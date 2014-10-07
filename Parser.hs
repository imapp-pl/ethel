module Parser where

import Data.Functor.Identity

import Text.Parsec 
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as E

import Syntax 

languageDef :: Lang.LanguageDef st
languageDef = Lang.emptyDef
              { Tok.commentStart = "/*"
              , Tok.commentEnd   = "*/"
              , Tok.commentLine  = "//"
              , Tok.nestedComments = True
              , Tok.identStart   = letter
              , Tok.identLetter  = alphaNum <|> oneOf "_'"
              , Tok.opStart      = oneOf "+-*/=()<>"
              , Tok.opLetter     = Tok.opStart languageDef
              , Tok.reservedNames = ["def", "in", "if", "then", "else"]
              , Tok.reservedOpNames = ["+", "-", "*", "/", "=",
                                       "==", "<>", "<", "<=", ">", ">="]
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


program :: Parser Program
program = do
  decls <- many1 declaration
  return $ Program decls

declaration :: Parser Declaration
declaration = do
  pos <- getPosition
  keyword "def"
  id <- identifier
  args <- many identifier
  operator "="
  body <- expression
  return $ makeDecl pos id args body

expression :: Parser Expression
expression =
  defExpression <|> ifExpression <|> numericExpression

defExpression :: Parser Expression
defExpression = do
  decls <- many declaration
  keyword "in"
  body <- expression
  return $ DefExpr decls body

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
numericExpression = E.buildExpressionParser table atomicExpression
  where table = [ [ prefix "-", prefix "+" ]
                , [ binary "*", binary "/" ]
                , [ binary "+", binary "-" ]
                , [ binary "==", binary "<>",
                    binary "<", binary "<=",
                    binary ">", binary ">=" ]
                ]
        prefix op = E.Prefix $ do { operator op;
                                    pos <- getPosition;
                                    return $ UnOpExpr pos op } 

        binary op = E.Infix ( do { operator op; return $ BinOpExpr op } )
                    E.AssocLeft

{-
primaryExpression :: Parser Expression
primaryExpression = do
  pos <- getPosition
  func <- identifier
  args <- many atomicExpression
  return $ case args of
    [] -> func
    _  -> CallExpr func args
-}

atomicExpression :: Parser Expression
atomicExpression = literalExpression
                   <|> varOrCallExpression
                   <|> between (operator "(") (operator ")") expression

varOrCallExpression :: Parser Expression
varOrCallExpression = do
  pos <- getPosition
  name <- identifier
  args <- many atomicExpression
  return $ case args of
    [] -> VarExpr pos name
    _  -> CallExpr pos name args

literalExpression :: Parser Expression
literalExpression = do
  pos <- getPosition
  num <- literal
  return $ LitExpr pos num

varExpression :: Parser Expression
varExpression = do
  pos <- getPosition
  name <- identifier
  return $ VarExpr pos name


parseString input =
  case runParser program () "source" input of
    Left err -> error $ show err
    Right p  -> p

parseFile path = do
  input <- readFile path
  let prog = parseString input
  putStr (show prog)
  
