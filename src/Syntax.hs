{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Syntax where

import qualified Data.Map as M
import           Data.Time (UTCTime)

import           Error
import           LitCust
import           Pretty
import           Type

type Name = String

data Expr = Expr Pos (Expr' Pos) deriving (Show, Eq)

pos :: Expr -> Pos
pos (Expr p _) = p

data Expr' a
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Let Name Expr Expr
    | List [Expr]
    | Lit Lit
    | Field Name (Maybe Expr) Type
    | If Expr Expr Expr
    | Op Binop Expr Expr
    deriving (Eq, Show)

data Lit
    = LNum   Double
    | LDate  UTCTime
    | LText  String
    | LTimUn TimeUnit
    | LWkSt  WeekStart
    | LBool  Bool
    deriving (Eq, Ord, Show)

type SynEnv = M.Map String Expr

instance Pretty Expr where
    ppr p (Expr _ x) = ppr p x

instance Pretty (Expr' Pos) where
    ppr _ (Var a)   = text a
    ppr p (App a b) = parensIf (p > 0) $ ppr (p+1) a <+> ppr p b
    ppr p (Lam a b) = text "\\" <> text a <+> text  "->" <+> ppr p b
    ppr p (Let a b c) = text "let" <> text a <+> text  "="
                    <+> ppr p b <+> text "in" <+> ppr p c
    ppr p (List xs) = text "[" <> hcat (punctuate comma (fmap (ppr p) xs)) <> text "]"
    ppr p (Lit a) = ppr p a
    ppr p (Op o a b) = parensIf (p>0) $ ppr p a <+> ppr p o <+> ppr p b
    ppr p (Field n v t) = text "field" <+> doubleQuotes (text n)
                      <+> text ":" <+> ppr p t <+> maybe (text "") val v
      where
        val x = text "as" <+> ppr p x
    ppr p (If a b c) = text "if" <+> ppr p a <+>
        text "then" <+> ppr p b <+>
        text "else" <+> ppr p c

instance Pretty Lit where
    ppr _ (LNum i)      = double i
    ppr _ (LBool True)  = text "True"
    ppr _ (LBool False) = text "False"
    ppr _ (LDate x)     = text "ISODate" <+> (doubleQuotes . text . show) x
    ppr _ (LText x)     = doubleQuotes . text $ x
    ppr p (LTimUn x)    = ppr p x
    ppr p (LWkSt x)     = ppr p x

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppdecl :: (String, Expr) -> String
ppdecl (a, b) = "let " ++ a ++ " = " ++ ppexpr b
