module Types where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
-- import           Control.Monad.State
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Time (UTCTime, formatTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Numeric (showFFloat)

import           Error
import           EvalState
import           LitCust
import           Pretty
import           Syntax
import           Type

-- | Evaluated values
data Value
    = VNum  Double
    | VBool Bool
    | VDate UTCTime
    | VText String
    | VTimUn TimeUnit
    | VWkSt  WeekStart
    | VList  [Value]
    | VField String Value Type
    | VClosure String Expr TermEnv
    | VBltIn String [Value]
    deriving Eq

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
    show (VWkSt  n)     = if n == Sunday then "SUNDAY" else "MONDAY"
    show (VList xs)     = "[" ++ intercalate "," (fmap show xs) ++ "]"
    show (VField x _ _) = show x
    show VClosure{}     = render . ppcolor Vivid White $ text "<<closure>>"
    show (VBltIn x _)   = show x

type TermEnv = M.Map String Value
-- type Interpreter e t = ExceptT e Identity t
type Interpreter e t = ExceptT e (ReaderT Est IO) t

-- | Eval errors
data EvalError
    = EvVarNotFound  String
    | EvFieldNoValue String
    | EvBuiltInError String
    deriving Eq

instance Show EvalError where
    show (EvVarNotFound a)  = "Not in scope: " ++ a
    show (EvFieldNoValue a) = "Field " ++ a ++ " is not assigned a value"
    show (EvBuiltInError a) = "Error: " ++ a

instance Error EvalError where
    showError = show

emptyTmenv :: TermEnv
emptyTmenv = M.empty

-- | Code generation types

data CgExpr
    = CGNum   Double
    | CGBool  Bool
    | CGDate  UTCTime
    | CGText  String
    | CGTimUn TimeUnit
    | CGWkSt  WeekStart
    | CGList  [CgExpr]
    | CGFld   String
    | CGOp    Binop CgExpr CgExpr
    | CGClos  String Expr CgEnv
    | CGIf    CgExpr CgExpr CgExpr
    | CGBltIn String [CgExpr]
    deriving (Show, Eq)

type CgEnv    = M.Map String CgExpr
type CodeGen e t = ExceptT e Identity t

-- | Code generation errors
data CodeGenError
    = CgVarNotFound String
    | CgUnboundVar String
    deriving Eq

instance Show CodeGenError where
    show (CgVarNotFound a) = "Not in scope: " ++ a
    show (CgUnboundVar a) = "Cannot emit expressions with unbound variables (\""
                         ++ a ++ "\" in particular).\nTry applying a field or \
                         \value to the expression"
instance Error CodeGenError where
    showError = show

-- | Built-in types
data BuiltIn = BuiltIn
    { evalVal :: [Value] -> Interpreter EvalError Value
    , emitVal :: [Value] -> String
    , typeSig :: Type
    , argHelp :: String
    , addHelp :: Maybe String
    }

