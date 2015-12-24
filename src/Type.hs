{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Type where

import qualified Data.Map as M
import           Data.Monoid (Monoid(..))
import           Prelude

import           Error
import           Pretty

newtype TVar = TV String
  deriving (Show, Eq, Ord)

instance Pretty TVar where
    ppr _ (TV x) = text x

data Type = TVar TVar     -- ^ Variable
          | TCon String   -- ^ Constant
          | TCns String Type -- ^ Type constructor
          | Type :-> Type -- ^ Arrow
          deriving (Eq, Ord, Show)

infixr 0 :->

instance Pretty Type where
    ppr p (a :-> b) = parensIf (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
      where
        isArrow (:->){} = True
        isArrow _ = False
    ppr p (TVar a) = ppcolor Vivid Magenta $ ppr p a
    ppr _ (TCon a) = ppcolor Vivid Blue $ text a
    ppr p (TCns x a) = ppcolor Vivid Blue $ text x <+> ppr p a

pptype :: Type -> String
pptype = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

instance Pretty Scheme where
    ppr p (Forall [] t) = ppr p t
    ppr p (Forall ts t) = text "forall"
                      <+> hcat (punctuate space exvars)
                      <>  text "." <+> ppr p t
      where
        exvars = fmap (ppcolor Vivid Magenta . ppr p) ts

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

newtype Subst = Subst (M.Map TVar Type)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Subst where
    ppr _ (Subst s) = vcat (punctuate space (map pprSub $ M.toList s))
      where pprSub (a, b) = ppr 0 a <+> text "~" <+> ppr 0 b

ppsubst :: Subst -> String
ppsubst = render . ppr 0

typeNum :: Type
typeNum  = TCon "Num"

typeDate :: Type
typeDate = TCon "Date"

typeText :: Type
typeText = TCon "Text"

typeBool :: Type
typeBool = TCon "Bool"

typeTimeUnit :: Type
typeTimeUnit = TCon "TimeUnit"

typeWeekStart :: Type
typeWeekStart = TCon "WeekStart"

typeA :: Type
typeA = TVar (TV "a")

typeB :: Type
typeB = TVar (TV "a")

typeList :: Type -> Type
typeList = TCns "List"

-- | Number of argument needed to fully apply a function
typeArgCnt :: Type -> Int
typeArgCnt x = cnt x - 1
  where
    cnt (_ :-> b) = 1 + cnt b
    cnt (TVar _)  = 1
    cnt (TCon _)  = 1
    cnt (TCns {}) = 1

-- | (Expected type, Actual type)
type Constraint = (Type, Type)

instance Pretty Constraint where
    ppr p (a, b) = ppr p a <+> text " ~ " <+> ppr p b

instance Pretty [Constraint] where
    ppr p cs = vcat (punctuate space (map (ppr p) cs))

ppconstraint :: Constraint -> String
ppconstraint = render . ppr 0

ppconstraints :: [Constraint] -> String
ppconstraints = render . ppr 0

-- | Inference errors
data TypeError
    = UnificationFail Pos Type Type
    | InfiniteType TVar Type
    | UnboundVariable Pos String
    | Ambigious [Constraint]
    | UnificationMismatch [Type] [Type]
    | EmptyList

instance Show TypeError where
    show (UnificationFail p a b) = "Type error at " ++ show p ++ ":\n" ++
                                   "Cannot unify types. Expected\n\t" ++
                                   pptype a ++ "\nbut got\n\t" ++ pptype b
    show (InfiniteType (TV a) b) = "Cannot construct the infinite type: " ++
                                   a ++ " = " ++ pptype b
    show (Ambigious cs) = concat ["Cannot not match expected type: '" ++
                                  pptype a ++ "' with actual type: '" ++
                                  pptype b ++ "'\n" | (a,b) <- cs]
    show (UnboundVariable p a) = "Error at " ++ show p ++ ":\n" ++
                                 "Not in scope: \"" ++ a ++ "\""
    show EmptyList = "Empty list"
    show (UnificationMismatch _ _) = "Unification Mismatch"

instance Error TypeError where
    showError = show
