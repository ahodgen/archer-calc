module BuiltIn.Common
    ( infError
    , biErr
    , vListToDbls
    ) where

import           Control.Monad.Except

import           Types

infError :: Interpreter EvalError a
infError = error "Received unexpected types. Either the built-in was \
                 \improperly defined or type inference failed."

biErr :: String -> Interpreter EvalError a
biErr = throwError . EvBuiltInError

vListToDbls :: [Value] -> Interpreter EvalError [Double]
vListToDbls = mapM v2d
  where
    v2d (VNum x) = return x
    v2d _        = infError
