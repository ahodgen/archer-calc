module Eval (runEval) where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map as M
import           Data.Monoid ((<>))

import           BuiltIn
import           Syntax
import           Types

eval :: TermEnv -> Expr -> Interpreter EvalError Value
eval env expr = case expr of
    Lit (LNum k)   -> return $ VNum k
    Lit (LBool k)  -> return $ VBool k
    Lit (LDate k)  -> return $ VDate k
    Lit (LText k)  -> return $ VText k
    Lit (LTimUn k) -> return $ VTimUn k

    Field k Nothing _  -> throwError $ EvFieldNoValue k
    Field _ (Just e) _ -> eval env e

    Var x -> case M.lookup x env of
        Just v -> return v
        Nothing -> case M.lookup x builtIns of
            Just _  -> evalBuiltIn $ VBltIn x []
            Nothing -> throwError $ EvVarNotFound x

    Op op a b -> do
        a' <- eval env a
        b' <- eval env b
        return $ binop op a' b'

    Lam x body -> return $ VClosure x body env

    App fun arg -> do
        argv <- eval env arg
        fun' <- eval env fun
        case fun' of
            VClosure x body clo -> do
                let nenv = M.insert x argv clo
                eval nenv body
            VBltIn x as -> evalBuiltIn $ VBltIn x (argv:as)
            _ -> error "Expression applied to a non-function. Something \
                       \went terribly wrong."

    Let x e body -> do
        e' <- eval env e
        let nenv = M.insert x e' env
        eval nenv body

    If cond tr fl -> do
        VBool br <- eval env cond
        if br then eval env tr else eval env fl

-- | Evaluate operators
binop :: Binop -> Value -> Value -> Value
binop Add (VNum a)  (VNum b)  = VNum  $ a +  b
binop Mul (VNum a)  (VNum b)  = VNum  $ a *  b
binop Sub (VNum a)  (VNum b)  = VNum  $ a -  b
binop Div (VNum a)  (VNum b)  = VNum  $ a /  b
binop Exp (VNum a)  (VNum b)  = VNum  $ a ** b
binop Eql (VNum a)  (VNum b)  = VBool $ a == b
binop Gt  (VNum a)  (VNum b)  = VBool $ a >  b
binop Gte (VNum a)  (VNum b)  = VBool $ a >= b
binop Lt  (VNum a)  (VNum b)  = VBool $ a <  b
binop Lte (VNum a)  (VNum b)  = VBool $ a <= b
binop Neq (VNum a)  (VNum b)  = VBool $ a /= b
binop Cat (VText a) (VText b) = VText $ a <> b
binop And (VBool a) (VBool b) = VBool $ a && b
binop Or  (VBool a) (VBool b) = VBool $ a || b
binop _ _ _ = error "Either an operator was added without an eval strategy, or\
                    \type inference didn't work :-("

runEval :: TermEnv -> String -> Expr -> Either EvalError (Value, TermEnv)
runEval env nm ex = runIdentity $ runExceptT $ do
    res <- eval env ex
    return (res, M.insert nm res env)
