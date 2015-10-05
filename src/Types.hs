module Types where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Time (UTCTime, formatTime)
import           Numeric
import           System.Locale (defaultTimeLocale)

import           Syntax
import           Type

-- | Evaluated values
data Value
    = VNum  Double
    | VBool Bool
    | VDate UTCTime
    | VText String
    | VTimUn TimeUnit
    | VField String Value Type
    | VClosure String Expr TermEnv
    | VBltIn String [Value]
    deriving Eq

type TermEnv = M.Map String Value
type Interpreter e t = ExceptT e Identity t

emptyTmenv :: TermEnv
emptyTmenv = M.empty

instance Show Value where
    -- If the number is within epsilon of an integer, show an integer.
    show (VNum n)   = if abs (n - realToFrac n') < eps
        then show (n' :: Integer)
        else showFFloat Nothing n ""
      where
        eps = 1.0e-8 :: Double
        n' = round n

    show (VBool n)  = show n
    show (VDate n)  = "ISODate \"" <> formatTime defaultTimeLocale "%FT%TZ" n
                   <> "\""
    show (VText n)  = show n
    show (VTimUn n) = show n
    show (VField x _ _) = show x
    show VClosure{} = "<<closure>>"
    show (VBltIn x _) = show x

-- | Code generation types

data CgExpr
    = CGNum   Double
    | CGBool  Bool
    | CGDate  UTCTime
    | CGText  String
    | CGTimUn TimeUnit
    | CGFld   String
    | CGOp    Binop CgExpr CgExpr
    | CGClos  String Expr CgEnv
    | CGIf    CgExpr CgExpr CgExpr
    | CGBltIn String [CgExpr]
    deriving (Show, Eq)

type CgEnv    = M.Map String CgExpr
type CodeGen e t = ExceptT e Identity t

-- | Built-in types
data BuiltIn = BuiltIn
    { evalVal :: [Value] -> Interpreter EvalError Value
    , emitVal :: String
    , typeSig  :: Type
    }

-- | Eval errors
data EvalError
    = EvVarNotFound  String
    | EvFieldNoValue String
    | EvBuiltInError String
    deriving Eq

-- | Code generation errors
data CodeGenError
    = CgVarNotFound String
    | CgUnboundVar String
    deriving Eq
