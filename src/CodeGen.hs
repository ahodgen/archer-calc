module CodeGen
    ( codeGen
    , runExprCg
    ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Time (formatTime, UTCTime(..))
import           Numeric
import           System.Locale (defaultTimeLocale)

import           BuiltIn
import           Optimize
import           Syntax
import           Types

exprToCG :: CgEnv -> Expr -> CodeGen CodeGenError CgExpr
exprToCG env expr = case expr of
    Lit (LNum k)   -> return $ CGNum k
    Lit (LBool k)  -> return $ CGBool k
    Lit (LDate k)  -> return $ CGDate k
    Lit (LText k)  -> return $ CGText k
    Lit (LTimUn k) -> return $ CGTimUn k

    Field k _ _ -> return $ CGFld k

    Var x -> case M.lookup x env of
        Just v -> return v
        Nothing -> case M.lookup x builtIns of
            Just _ -> return $ CGBltIn x []
            Nothing -> error $ "Variable " <> x <> " not found :-("

    Op op a b -> do
        a' <- exprToCG env a
        b' <- exprToCG env b
        return $ CGOp op a' b'

    Lam x body -> return $ CGClos x body env

    App fun arg -> do
        fun' <- exprToCG env fun
        argv <- exprToCG env arg
        case fun' of
            CGClos  x body scp -> exprToCG scope body
              where
                scope = M.insert x argv scp
            CGBltIn x as -> return $ CGBltIn x (argv:as)
            _ -> error "Expression applied to a non-function. Something \
                       \went terribly wrong."

    Let x e body -> do
        e' <- exprToCG env e
        let scope = M.insert x e' env
        exprToCG scope body

    If cond tr fl -> do
        cond' <- exprToCG env cond
        tr'   <- exprToCG env tr
        fl'   <- exprToCG env fl
        return $ CGIf cond' tr' fl'

emitCG :: CgEnv -> CgExpr -> CodeGen CodeGenError String
emitCG  env expr = case expr of
    CGNum  k -> return $ if abs (k - realToFrac k') < eps
        then show (k' :: Integer)
        else showFFloat Nothing k ""
      where
        eps = 1.0e-8 :: Double
        k' = round k

    CGBool k -> return $ show k
    CGDate k -> return $ if utctDayTime k == 0
        then "\"" <> formatTime defaultTimeLocale "%m/%d/%Y" k <> "\""
        else "\"" <> formatTime defaultTimeLocale "%m/%d/%Y %T" k <> "\""
    CGText k -> return $ show k
    CGTimUn Day -> return "DAY"
    CGTimUn Hour -> return "HOUR"
    CGTimUn Min -> return "MINUTE"

    CGOp op a b -> do
        a' <- emitCG env a
        b' <- emitCG env b
        return $ emitOp op a' b'

    CGIf cond tr fl -> do
        cond' <- emitCG env cond
        tr'   <- emitCG env tr
        fl'   <- emitCG env fl
        return $ "IF(" <> cond' <> "," <> tr' <> "," <> fl' <> ")"

    CGFld x -> return $ "[" <> x <> "]"

    CGClos x _ _ -> throwError $ CgUnboundVar x

    CGBltIn x xs -> do
        exs <- mapM (emitCG env) xs
        return $ emitBuiltIn x exs

-- | Emit binary operators
emitOp :: Binop -> String -> String -> String
emitOp Add a b = a <> "+" <> b
emitOp Mul a b = a <> "*" <> b
emitOp Sub a b = a <> "-" <> b
emitOp Div a b = a <> "/" <> b
emitOp Exp a b = a <> "^" <> b
emitOp Eql a b = a <> "=" <> b
emitOp Gt  a b = a <> ">" <> b
emitOp Gte a b = a <> ">=" <> b
emitOp Lt  a b = a <> "<" <> b
emitOp Lte a b = a <> "<=" <> b
emitOp Neq a b = a <> "<>" <> b
emitOp Cat a b = a <> "&" <> b
emitOp And a b = "AND(" <> a <> "," <> b <> ")"
emitOp Or  a b = "OR(" <> a <> "," <> b <> ")"

codeGen :: CgEnv -> Expr -> Either CodeGenError String
codeGen env expr = runIdentity . runExceptT $
     emitCG env =<< (optimize env <$> exprToCG env expr)

runExprCg :: CgEnv -> Expr -> Either CodeGenError CgExpr
runExprCg env expr = runIdentity . runExceptT $ exprToCG env expr
