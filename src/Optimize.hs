module Optimize
    ( optimize
    , CgExpr (..)
    , CgEnv
    ) where

import           Data.Monoid ((<>))

import           Syntax
import           Types

-- | Folding of operators
foldOp :: CgEnv -> Binop -> CgExpr -> CgExpr -> CgExpr
foldOp _ Add (CGNum a)  (CGNum b)  = CGNum (a+b)
foldOp e Add (CGNum 0)  b          = constFold e b -- 0 + b = b
foldOp e Add a          (CGNum 0)  = constFold e a -- a + 0 = 0

foldOp _ Mul (CGNum a)  (CGNum b)  = CGNum (a*b)
foldOp e Mul a          (CGNum 1)  = constFold e a -- a * 1 = a
foldOp e Mul (CGNum 1)  b          = constFold e b -- 1 * b = b
foldOp _ Mul _          (CGNum 0)  = CGNum 0       -- x * 0 = 0
foldOp _ Mul (CGNum 0)  _          = CGNum 0       -- 0 * x = 0

foldOp _ Sub (CGNum a)  (CGNum b)  = CGNum (a-b)
foldOp e Sub a          (CGNum 0)  = constFold e a -- a - 0 = 0

foldOp _ Div (CGNum 0)  _          = CGNum 0       -- 0 / x = 0

foldOp e Exp (CGOp Exp a (CGNum b)) (CGNum c)
          = CGOp Exp (constFold e a) (CGNum (b*c)) -- (a^b)^c = a^(b*c)
foldOp _ Exp (CGNum a)  (CGNum b)  = CGNum (a**b)
foldOp e Exp a          (CGNum 1)  = constFold e a -- a ^ 1 = a
foldOp _ Exp _          (CGNum 0)  = CGNum 1       -- a ^ 0 = 1

foldOp _ Eql (CGNum a)  (CGNum b)  = CGBool (a == b)
foldOp _ Eql (CGText a) (CGText b) = CGBool (a == b)
foldOp _ Eql (CGDate a) (CGDate b) = CGBool (a == b)
foldOp _ Eql (CGBool a) (CGBool b) = CGBool (a == b)
foldOp _ Eql (CGFld a)  (CGFld b)  = CGBool (a == b)

foldOp _ Neq (CGNum a)  (CGNum b)  = CGBool (a /= b)
foldOp _ Neq (CGText a) (CGText b) = CGBool (a /= b)
foldOp _ Neq (CGDate a) (CGDate b) = CGBool (a /= b)
foldOp _ Neq (CGBool a) (CGBool b) = CGBool (a /= b)
foldOp _ Neq (CGFld a)  (CGFld b)  = CGBool (a /= b)

-- XXX: Bool in Ord Funcs?
foldOp _ Gt  (CGNum a)  (CGNum b)  = CGBool (a > b)
foldOp _ Gt  (CGText a) (CGText b) = CGBool (a > b)
foldOp _ Gt  (CGDate a) (CGDate b) = CGBool (a > b)
foldOp _ Gte (CGNum a)  (CGNum b)  = CGBool (a >= b)
foldOp _ Gte (CGText a) (CGText b) = CGBool (a >= b)
foldOp _ Gte (CGDate a) (CGDate b) = CGBool (a >= b)
foldOp _ Lt  (CGNum a)  (CGNum b)  = CGBool (a < b)
foldOp _ Lt  (CGText a) (CGText b) = CGBool (a < b)
foldOp _ Lt  (CGDate a) (CGDate b) = CGBool (a < b)
foldOp _ Lte (CGNum a)  (CGNum b)  = CGBool (a <= b)
foldOp _ Lte (CGText a) (CGText b) = CGBool (a <= b)
foldOp _ Lte (CGDate a) (CGDate b) = CGBool (a <= b)

foldOp _ Cat (CGText a) (CGText b) = CGText (a <> b)
foldOp e Cat (CGText "") b         = constFold e b
foldOp e Cat a          (CGText "") = constFold e a

foldOp _ And (CGBool False) _      = CGBool False   -- F ∧ Q = F
foldOp _ And _      (CGBool False) = CGBool False   -- P ∧ F = F
foldOp e And (CGBool True)  q      = constFold e q  -- T ∧ Q = Q
foldOp e And p       (CGBool True) = constFold e p  -- P ∧ T = P

foldOp _ Or  (CGBool p) (CGBool q) = CGBool (p || q)
foldOp _ Or  (CGBool True) _       = CGBool True    -- T ∨ Q = T
foldOp _ Or  _       (CGBool True) = CGBool True    -- P ∨ T = T
foldOp e Or  (CGBool False) q      = constFold e q  -- F ∨ Q = Q
foldOp e Or  p      (CGBool False) = constFold e p  -- P ∨ F = P

foldOp e x a b = CGOp x (constFold e a) (constFold e b)

-- | Fold a conditional
foldCond :: CgEnv -> CgExpr -> CgExpr -> CgExpr -> CgExpr
foldCond env (CGBool True)  a   _              = constFold env a
foldCond env (CGBool False) _   b              = constFold env b
foldCond env cond (CGBool True) (CGBool False) = constFold env cond
foldCond env x              a   b              = CGIf (constFold env x)
                                                      (constFold env a)
                                                      (constFold env b)

-- | Constant fold an expression
constFold :: CgEnv -> CgExpr -> CgExpr
constFold env expr = case expr of
    CGOp x a b -> foldOp   env x a b
    CGIf x a b -> foldCond env x a b
    x -> x

-- | Recursively run a pass until the current input matches
--   the last output.
recurseEqulib :: (CgEnv -> CgExpr -> CgExpr) -> CgEnv -> CgExpr -> CgExpr
recurseEqulib f te ex = go ex (CGNum 0)
  where
    go cur lst | cur == lst = cur
               | otherwise  = go (f te cur) cur

-- | Run all of our optimizer passes (only one for now).
optimize :: CgEnv -> CgExpr -> CgExpr
optimize = recurseEqulib constFold
