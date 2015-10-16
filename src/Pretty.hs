{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty
    ( ppconstraint
    , ppconstraints
    , ppdecl
    , ppenv
    , ppexpr
    , ppscheme
    , ppsubst
    , ppsignature
    , pptype
    , prompt
    ) where

import qualified Data.Map as Map
import           Data.Time (formatTime)
import           Numeric (showFFloat)
import           System.Console.ANSI
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Text.PrettyPrint

import           Env
import           Infer
import           Syntax
import           Type
import           Types

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Name where
    ppr _ = text

instance Pretty TVar where
    ppr _ (TV x) = text x

instance Pretty Type where
    ppr p (a :-> b) = parensIf (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
      where
        isArrow (:->){} = True
        isArrow _ = False
    ppr p (TVar a) = ppcolor Vivid Magenta $ ppr p a
    ppr _ (TCon a) = ppcolor Vivid Blue $ text a

instance Pretty Scheme where
    ppr p (Forall [] t) = ppr p t
    ppr p (Forall ts t) = text "forall"
                      <+> hcat (punctuate space exvars)
                      <>  text "." <+> ppr p t
      where
        exvars = fmap (ppcolor Vivid Magenta . ppr p) ts

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

instance Pretty Expr where
    ppr p (Var a) = ppr p a
    ppr p (App a b) = parensIf (p > 0) $ ppr (p+1) a <+> ppr p b
    ppr p (Lam a b) = text "\\" <> ppr p a <+> text  "->" <+> ppr p b
    ppr p (Let a b c) = text "let" <> ppr p a <+> text  "="
                    <+> ppr p b <+> text "in" <+> ppr p c
    ppr p (Lit a) = ppr p a
    ppr p (Op o a b) = parensIf (p>0) $ ppr p a <+> ppr p o <+> ppr p b
    ppr p (Field n v t) = text "field" <+> doubleQuotes (ppr p n)
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
    ppr _ (LTimUn Day)  = text "DAY"
    ppr _ (LTimUn Hour) = text "HOUR"
    ppr _ (LTimUn Min)  = text "MINUTE"

instance Pretty Constraint where
    ppr p (a, b) = ppr p a <+> text " ~ " <+> ppr p b

instance Pretty [Constraint] where
    ppr p cs = vcat (punctuate space (map (ppr p) cs))

instance Pretty Subst where
    ppr _ (Subst s) = vcat (punctuate space (map pprSub $ Map.toList s))
      where pprSub (a, b) = ppr 0 a <+> text "~" <+> ppr 0 b

instance Show TypeError where
    show (UnificationFail a b) = concat ["Cannot unify types: \n\t"
                                        , pptype a, "\nwith \n\t", pptype b]
    show (InfiniteType (TV a) b) = concat ["Cannot construct the infinite\
                                          \ type: ", a, " = ", pptype b]
    show (Ambigious cs) = concat ["Cannot not match expected type: '" ++
                                  pptype a ++ "' with actual type: '" ++
                                  pptype b ++ "'\n" | (a,b) <- cs]
    show (UnboundVariable a) = "Not in scope: " ++ a
    show (UnificationMismatch _ _) = "Unification Mismatch"

instance Show EvalError where
    show (EvVarNotFound a)  = "Not in scope: " ++ a
    show (EvFieldNoValue a) = "Field " ++ a ++ " is not assigned a value"
    show (EvBuiltInError a) = "Error: " ++ a

instance Show CodeGenError where
    show (CgVarNotFound a) = "Not in scope: " ++ a
    show (CgUnboundVar a) = "Cannot emit expressions with unbound variables (\""
                         ++ a ++ "\" in particular).\nTry applying a field or \
                         \value to the expression"

instance Show Value where
    -- If the number is within epsilon of an integer, show an integer.
    show (VNum n)   = if abs (n - realToFrac n') < eps
        then show (n' :: Integer)
        else showFFloat Nothing n ""
      where
        eps = 1.0e-8 :: Double
        n' = round n

    show (VBool n)      = show n
    show (VDate n)      = "ISODate \"" ++ formatTime defaultTimeLocale "%FT%TZ"
                          n ++ "\""
    show (VText n)      = show n
    show (VTimUn n)     = show n
    show (VField x _ _) = show x
    show VClosure{}     = render . ppcolor Vivid White $ text "<<closure>>"
    show (VBltIn x _)   = show x

ppcolor :: ColorIntensity -> Color -> Doc -> Doc
ppcolor insty color x = text (setSGRCode [SetColor Foreground insty color])
                     <> x
                     <> text (setSGRCode [Reset])

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b

ppdecl :: (String, Expr) -> String
ppdecl (a, b) = "let " ++ a ++ " = " ++ ppexpr b

ppenv :: Env -> [String]
ppenv (TypeEnv env) = map ppsignature $ Map.toList env

ppconstraint :: Constraint -> String
ppconstraint = render . ppr 0

ppconstraints :: [Constraint] -> String
ppconstraints = render . ppr 0

ppsubst :: Subst -> String
ppsubst = render . ppr 0

prompt :: String
prompt = setSGRCode [SetConsoleIntensity BoldIntensity]
      ++ "ArcherCalc> "
      ++ setSGRCode [Reset]
