{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn #-}
module Parser
    ( parseExpr
    , parseModule
    ) where

import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as L
import           Data.Time (parseTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Text.Parsec
import qualified Text.Parsec.Expr as EX
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token as Tok

import           Error
import           LitCust
import           Lexer
import           Syntax
import           Type

getMyPosition :: Parser Pos
getMyPosition = fmap fromSourcePos getPosition

addLocation :: Parser (Expr' Pos) -> Parser Expr
addLocation ex = do
    (Pos start _, e, Pos _ end) <- located ex
    return (at start end e)
  where
    at s e = Expr (Pos s e)

located :: Parser a -> Parser (Pos, a, Pos)
located par = do
    start <- getMyPosition
    value <- par
    end <- getMyPosition
    return (start, value, end)

natOrFloat :: Parser (Either Integer Double)
natOrFloat = Tok.naturalOrFloat lexer

data Sign = Positive | Negative

sign  :: Parser Sign
sign  = (char '-' >> return Negative)
    <|> (char '+' >> return Positive)
    <|> return Positive

number :: Parser Expr
number = addLocation $ do
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
text = addLocation $ do
        x <- stringLit
        return (Lit (LText x))

list :: Parser Expr
list =
    addLocation $ do
        _ <- char '['
        xs <- commaSep expr
        _ <- char ']'
        return $ List xs

variable :: Parser Expr
variable = addLocation $ do
        x <- identifier
        return (Var x)

isoDate :: Parser Expr
isoDate = addLocation $ do
    reserved "ISODate"
    t <- stringLit
    parDate "%FT%T%Z" t <|> parDate "%F" t

archDate :: Parser Expr
archDate = addLocation $ do
    reserved "ArDate"
    t <- stringLit
    parDate "%-m/%-d/%Y %-H:%-M:%-S %p" t
        <|> parDate "%-m/%-d/%Y %-H:%-M %p" t
        <|> parDate "%-m/%-d/%Y %-H:%-M:%-S" t
        <|> parDate "%-m/%-d/%Y %-H:%-M" t
        <|> parDate "%-m/%-d/%Y" t

parDate :: String -> String -> Parser (Expr' Pos)
parDate x d = case parseTime defaultTimeLocale x d of
    Just y  -> return (Lit (LDate y))
    Nothing -> unexpected "date format"

date :: Parser Expr
date = isoDate <|> archDate

bool :: Parser Expr
bool = addLocation $
        (reserved "True"  >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

timeUnit :: Parser Expr
timeUnit = addLocation $
           (reserved "Day"    >> return (Lit (LTimUn Day)))
       <|> (reserved "Hour"   >> return (Lit (LTimUn Hour)))
       <|> (reserved "Minute" >> return (Lit (LTimUn Min)))

weekStart :: Parser Expr
weekStart = addLocation $
            (reserved "Sunday" >> return (Lit (LWkSt Sunday)))
        <|> (reserved "Monday" >> return (Lit (LWkSt Monday)))

foldEx :: (a -> Expr -> Expr' Pos) -> Expr -> [a] -> Expr
foldEx _ z [] = z
foldEx f z@(Expr p _) (x:xs) = Expr p $ f x (foldEx f z xs)

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldEx Lam body args

letin :: Parser Expr
letin = addLocation $ do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let x e1 e2)

field :: Parser Expr
field = addLocation $ do
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

ifthen :: Parser Expr
ifthen = addLocation $ do
    reserved "if"
    cond <- aexp
    reservedOp "then"
    tr <- aexp
    reserved "else"
    fl <- aexp
    return (If cond tr fl)

aexp :: Parser Expr
aexp =    parens expr
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
term = EX.buildExpressionParser opTable aexp

infixOp :: String -> (a -> a -> a) -> EX.Assoc -> Op a
infixOp x f = EX.Infix (reservedOp x >> return f)

opTable :: Operators Expr
opTable =
    [ [ infixOp "^"  (exOp Exp) EX.AssocLeft
      ]
    , [ infixOp "*"  (exOp Mul) EX.AssocLeft
      , infixOp "/"  (exOp Div) EX.AssocLeft
      ]
    , [ infixOp "+"  (exOp Add) EX.AssocLeft
      , infixOp "-"  (exOp Sub) EX.AssocLeft
      ]
    , [ infixOp "&"  (exOp Cat) EX.AssocLeft
      ]
    , [ infixOp "==" (exOp Eql) EX.AssocLeft
      , infixOp ">"  (exOp Gt)  EX.AssocLeft
      , infixOp ">=" (exOp Gte) EX.AssocLeft
      , infixOp "<"  (exOp Lt)  EX.AssocLeft
      , infixOp "<=" (exOp Lte) EX.AssocLeft
      , infixOp "<>" (exOp Neq) EX.AssocLeft
      ]
    , [ infixOp "&&" (exOp And) EX.AssocLeft
      , infixOp "||" (exOp Or)  EX.AssocLeft
      ]
    ]
  where
    exOp :: Binop -> Expr -> Expr -> Expr
    exOp o a@(Expr (Pos s _) _) b@(Expr (Pos _ e) _) = Expr (Pos s e) (Op o a b)

expr :: Parser Expr
expr = do
    es <- many1 term
    return $ fold1Ex App es
  where
    fold1Ex :: (Expr -> Expr -> Expr' Pos) -> [Expr] -> Expr
    fold1Ex f xs = fromMaybe (error "fold1Ex: empty structure")
                    (foldl mf Nothing xs)
     where
        mf m y@(Expr (Pos _ e) _) = Just $ case m of
                    Nothing -> y
                    Just x@(Expr (Pos s _) _) -> Expr (Pos s e) (f x y)


type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
    reserved "let"
    name <- identifier <?> "name"
    args <- many identifier
    reservedOp "="
    body <- expr <?> "an expression"
    return (name, foldEx Lam body args)

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
