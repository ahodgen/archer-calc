module LitCust where

import Pretty

-- Custom literal types used in multiple ASTs

data TimeUnit
    = Day
    | Hour
    | Min
    deriving (Eq, Ord, Show)

instance Pretty TimeUnit where
    ppr _ Day  = text "Day"
    ppr _ Hour = text "Hour"
    ppr _ Min  = text "Minute"

data WeekStart
    = Sunday
    | Monday
    deriving (Eq, Ord, Show)

instance Pretty WeekStart where
    ppr _ Sunday = text "Sunday"
    ppr _ Monday = text "Monday"

data Binop
    = Add | Sub | Mul | Div | Exp | Eql | Gt
    | Gte | Lt | Lte | Neq | And | Or | Cat
    deriving (Eq, Ord, Show)

instance Pretty Binop where
    ppr _ Add = text "+"
    ppr _ Sub = text "-"
    ppr _ Mul = text "*"
    ppr _ Div = text "/"
    ppr _ Exp = text "^"
    ppr _ Eql = text "=="
    ppr _ Gt  = text ">"
    ppr _ Gte = text ">="
    ppr _ Lt  = text "<"
    ppr _ Lte = text "<="
    ppr _ Neq = text "<>"
    ppr _ Cat = text "&"
    ppr _ And = text "&&"
    ppr _ Or  = text "||"
