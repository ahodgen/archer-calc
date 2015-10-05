module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type = TVar TVar     -- ^ Variable
          | TCon String   -- ^ Constant
          | Type :-> Type -- ^ Arrow
          deriving (Show, Eq, Ord)

infixr 0 :->

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

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

typeA :: Type
typeA = TVar (TV "a")

typeB :: Type
typeB = TVar (TV "a")

-- | Number of argument needed to fully apply a function
typeArgCnt :: Type -> Int
typeArgCnt x = cnt x - 1
  where
    cnt (_ :-> b) = 1 + cnt b
    cnt (TVar _)  = 1
    cnt (TCon _)  = 1

-- | (Expected type, Actual type)
type Constraint = (Type, Type)

-- | Inference errors
data TypeError
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable String
    | Ambigious [Constraint]
    | UnificationMismatch [Type] [Type]
