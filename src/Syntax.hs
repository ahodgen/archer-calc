module Syntax where

import qualified Data.Map as M
import           Data.Time (UTCTime)
import           Type

type Name = String

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Let Name Expr Expr
    | Lit Lit
    | Field Name (Maybe Expr) Type
    | If Expr Expr Expr
    | Op Binop Expr Expr
    deriving (Eq, Ord, Show)

data Lit
    = LNum   Double
    | LDate  UTCTime
    | LText  String
    | LTimUn TimeUnit
    | LWkSt  WeekStart
    | LBool  Bool
    deriving (Eq, Ord, Show)

data Binop
    = Add | Sub | Mul | Div | Exp | Eql | Gt
    | Gte | Lt | Lte | Neq | And | Or | Cat
    deriving (Eq, Ord, Show)

data TimeUnit
    = Day
    | Hour
    | Min
    deriving (Eq, Ord, Show)

data WeekStart
    = Sunday
    | Monday
    deriving (Eq, Ord, Show)

type SynEnv = M.Map String Expr
