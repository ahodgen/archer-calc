module BuiltIn.Logic (builtInLogic) where

import qualified Data.Map as M

import           Type
import           Types

infError :: Interpreter EvalError Value
infError = error "Received unexpected types. Either the built-in was \
                 \improperly defined or type inference failed."

biLogNot :: BuiltIn
biLogNot = BuiltIn
    { evalVal = logNot
    , emitVal = const "NOT"
    , typeSig = typeBool :-> typeBool
    , argHelp = "logical_test"
    , addHelp = Nothing
    }

logNot :: [Value] -> Interpreter EvalError Value
logNot [VBool x] = return . VBool $ not x
logNot _         = infError

builtInLogic :: M.Map String BuiltIn
builtInLogic = M.fromList
    [ ("not", biLogNot)
    ]
