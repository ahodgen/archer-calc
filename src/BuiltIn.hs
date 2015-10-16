module BuiltIn
    ( builtIns
    , builtInHelp
    , emitBuiltIn
    , evalBuiltIn
    , envBuiltIn
    ) where

import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Monoid ((<>))

import BuiltIn.Date
import BuiltIn.Logic
import BuiltIn.Math
import BuiltIn.Stats
import BuiltIn.Text
import Env
import Type
import Types

builtIns :: M.Map String BuiltIn
builtIns = builtInDate  <> builtInLogic <> builtInMath
        <> builtInStats <> builtInText

builtInHelp :: M.Map String String
builtInHelp = helpDate

evalBuiltIn :: Value -> Interpreter EvalError Value
evalBuiltIn v@(VBltIn nm xs) = if typeArgCnt (typeSig b) == length xs then
        evalVal b $ reverse xs
    else
        return v
  where
    b = builtIns M.! nm
evalBuiltIn _ = error "evalBuiltIn was not passed a built-in. Something went \
                      \horribly awry."

emitBuiltIn :: String -> [String] -> String
emitBuiltIn nm vs = emitVal b <> "(" <> args <> ")"
  where
    b = builtIns M.! nm
    args = intercalate "," $ reverse vs

envBuiltIn :: Env
envBuiltIn = TypeEnv . M.fromList . fmap toScheme $ M.toList builtIns
  where
    toScheme (nm, x) = (nm, Forall [] (typeSig x))
