module Types where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map as M
import           Data.Time (UTCTime)

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
    deriving (Eq, Ord)

type TermEnv = M.Map String Value
type Interpreter e t = ExceptT e Identity t

-- | Eval errors
data EvalError
    = EvVarNotFound  String
    | EvFieldNoValue String
    | EvBuiltInError String
    deriving Eq

emptyTmenv :: TermEnv
emptyTmenv = M.empty

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

-- | Code generation errors
data CodeGenError
    = CgVarNotFound String
    | CgUnboundVar String
    deriving Eq

-- | Built-in types
data BuiltIn = BuiltIn
    { evalVal :: [Value] -> Interpreter EvalError Value
    , emitVal :: String
    , typeSig  :: Type
    }

