{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn #-}
module Parser
    ( parseExpr
    , parseModule
    ) where

import qualified Data.Text.Lazy as L
import           Data.Time (parseTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Text.Parsec
import qualified Text.Parsec.Expr as Ex
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token as Tok

import           Lexer
import           Syntax
import           Type

natOrFloat :: Parser (Either Integer Double)
natOrFloat = Tok.naturalOrFloat lexer

data Sign = Positive | Negative

sign  :: Parser Sign
sign  = (char '-' >> return Negative)
    <|> (char '+' >> return Positive)
    <|> return Positive

number :: Parser Expr
number = do
    s <- sign
    num <- natOrFloat
    return . Lit . LNum $ either (apSign s . fromIntegral) (apSign s) num
  where
    apSign Positive =  id
    apSign Negative =  negate

stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

text :: Parser Expr
text = do
    x <- stringLit
    return (Lit (LText x))

list :: Parser Expr
list = do
    _ <- char '['
    xs <- commaSep expr
    _ <- char ']'
    return (List xs)

variable :: Parser Expr
variable = do
    x <- identifier
    return (Var x)

isoDate :: Parser Expr
isoDate = do
    reserved "ISODate"
    t <- stringLit
    parDate "%FT%T%Z" t <|> parDate "%F" t

archDate :: Parser Expr
archDate = do
    reserved "ArDate"
    t <- stringLit
    parDate "%-m/%-d/%Y %-H:%-M:%-S %p" t
        <|> parDate "%-m/%-d/%Y %-H:%-M %p" t
        <|> parDate "%-m/%-d/%Y %-H:%-M:%-S" t
        <|> parDate "%-m/%-d/%Y %-H:%-M" t
        <|> parDate "%-m/%-d/%Y" t

parDate :: String -> String -> Parser Expr
parDate x d = case parseTime defaultTimeLocale x d of
    Just y  -> return (Lit (LDate y))
    Nothing -> unexpected "date format"

date :: Parser Expr
date = isoDate <|> archDate

bool :: Parser Expr
bool =  (reserved "True"  >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

timeUnit :: Parser Expr
timeUnit = (reserved "Day"    >> return (Lit (LTimUn Day)))
       <|> (reserved "Hour"   >> return (Lit (LTimUn Hour)))
       <|> (reserved "Minute" >> return (Lit (LTimUn Min)))

weekStart :: Parser Expr
weekStart = (reserved "Sunday" >> return (Lit (LWkSt Sunday)))
        <|> (reserved "Monday" >> return (Lit (LWkSt Monday)))

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body args

letin :: Parser Expr
letin = do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let x e1 e2)

field :: Parser Expr
field = do
    reserved "field"
    fld <- stringLit
    reservedOp ":" <?> "a type declaration"
    typ <- fieldType
    e <- optionMaybe $ do
            reserved "as"
            expr
    return $ Field fld e typ

fieldType :: Parser Type
fieldType = (reserved "Bool"  >> return typeBool)
        <|> (reserved "Date"  >> return typeDate)
        <|> (reserved "Num"   >> return typeNum)
        <|> (reserved "Text"  >> return typeText)
        <|> (reserved "List"  >> fmap typeList fieldType)
        <|> (reserved "Maybe" >> fmap typeMaybe fieldType)

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- aexp
    reservedOp "then"
    tr <- aexp
    reserved "else"
    fl <- aexp
    return (If cond tr fl)

aexp :: Parser Expr
aexp =  parens expr
    <|> date
    <|> bool
    <|> timeUnit
    <|> weekStart
    <|> number
    <|> text
    <|> ifthen
    <|> field
    <|> letin
    <|> lambda
    <|> list
    <|> variable

term :: Parser Expr
term = Ex.buildExpressionParser opTable aexp

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

opTable :: Operators Expr
opTable =
    [ [ infixOp "^"  (Op Exp) Ex.AssocLeft
      ]
    , [ infixOp "*"  (Op Mul) Ex.AssocLeft
      , infixOp "/"  (Op Div) Ex.AssocLeft
      ]
    , [ infixOp "+"  (Op Add) Ex.AssocLeft
      , infixOp "-"  (Op Sub) Ex.AssocLeft
      ]
    , [ infixOp "&"  (Op Cat) Ex.AssocLeft
      ]
    , [ infixOp "==" (Op Eql) Ex.AssocLeft
      , infixOp ">"  (Op Gt)  Ex.AssocLeft
      , infixOp ">=" (Op Gte) Ex.AssocLeft
      , infixOp "<"  (Op Lt)  Ex.AssocLeft
      , infixOp "<=" (Op Lte) Ex.AssocLeft
      , infixOp "<>" (Op Neq) Ex.AssocLeft
      ]
    , [ infixOp "&&" (Op And) Ex.AssocLeft
      , infixOp "||" (Op Or)  Ex.AssocLeft
      ]
    ]

expr :: Parser Expr
expr = do
    es <- many1 term
    return (foldl1 App es)

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
    reserved "let"
    name <- identifier
    args <- many identifier
    reservedOp "="
    body <- expr
    return (name, foldr Lam body args)

val :: Parser Binding
val = do
    ex <- expr
    return ("it", ex)

decl :: Parser Binding
decl = letdecl <|> val

top :: Parser Binding
top = do
    x <- decl
    optional semi
    return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule fname input = parse (contents modl) fname input
