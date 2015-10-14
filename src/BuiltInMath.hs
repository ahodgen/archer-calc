module BuiltInMath (builtInMath) where

import           Control.Monad.Except
import qualified Data.Map as M

import           Type
import           Types

-- TODO:
-- SUMIF (REFS need implementing)
-- SUMPRODUCT
-- POWER (use ^)
-- PRODUCT (need to implement lists)
-- RAND
-- SUMPRODUCT
-- SUMX2MY2
-- SUMX2PY2
-- SUMXMY2

infError :: Interpreter EvalError Value
infError = error "Received unexpected types. Either the built-in was \
                 \improperly defined or type inference failed."

-- | Run a haskell unary function for a math built-in.
unaryBiHask :: (Double -> Double) -> [Value] -> Interpreter EvalError Value
unaryBiHask f [VNum x] = return . VNum . f $ x
unaryBiHask _ _        = infError

weirdRound :: (Double -> Integer) -> (Double -> Integer) -> Double -> Double
           -> Interpreter EvalError Value
weirdRound fneg fpos n d
    | d < 0     = return . VNum $ (intToDbl . f $ n / dig) * dig
    | otherwise = return . VNum $ (intToDbl . f $ n * dig) / dig
  where
    dig = 10 ** abs d
    f | n < 0     = fneg
      | otherwise = fpos

data EvenOdd = Even | Odd

evenOdd :: EvenOdd -> Double -> Double
evenOdd eo x
    | x >= 0    = 2 * intToDbl (ceiling $ (x+off) / 2) - off
    | otherwise = 2 * intToDbl (floor $ (x+off) / 2) - off
  where
    off = case eo of
        Even -> 0
        Odd  -> 1

intToDbl :: Integer -> Double
intToDbl = fromIntegral

mAbs, mAcos, mAcosh, mAsin, mAsinh, mAtan, mAtanh, mCos, mCosh, mDeg, mEven,
      mExp, mInt, mLn, mLog10, mOdd, mRad, mSign, mSin, mSinh, mTan, mTanh
      :: [Value] -> Interpreter EvalError Value
mAbs   = unaryBiHask abs
mAcos  = unaryBiHask acos
mAcosh = unaryBiHask acosh
mAsin  = unaryBiHask asin
mAsinh = unaryBiHask asinh
mAtan  = unaryBiHask atan
mAtanh = unaryBiHask atanh
mCos   = unaryBiHask cos
mCosh  = unaryBiHask cosh
mDeg   = unaryBiHask (\x -> 180*x/pi)
mEven  = unaryBiHask $ evenOdd Even
mExp   = unaryBiHask exp
mInt   = unaryBiHask (\x -> intToDbl $ truncate (if x < 0 then x-1 else x))
mLn    = unaryBiHask log
mOdd   = unaryBiHask $ evenOdd Odd
mLog10 = unaryBiHask (logBase 10)
mRad   = unaryBiHask (\x -> x*pi/180)
mSign  = unaryBiHask sign
  where
    sign x | x > 0     = 1
           | x < 0     = -1
           | otherwise = 0
mSin   = unaryBiHask sin
mSinh  = unaryBiHask sinh
mTan   = unaryBiHask tan
mTanh  = unaryBiHask tanh

mAtan2 :: [Value] -> Interpreter EvalError Value
mAtan2 [VNum x, VNum y] = return . VNum $ atan2 y x
mAtan2 _ = infError

mCeil :: [Value] -> Interpreter EvalError Value
mCeil [VNum x, VNum sig] = return . VNum $ sig * intToDbl (ceiling (x/sig))
mCeil _ = infError

mComb :: [Value] -> Interpreter EvalError Value
mComb [VNum n, VNum r] = return $ VNum x
  where
    x = intToDbl (fact n) / intToDbl (fact r * fact (n-r))
    fact y = product [1..floor y]
mComb _ = infError

mFact :: [Value] -> Interpreter EvalError Value
mFact [VNum n] | n >= 0    = return . VNum . intToDbl $ product [1..floor n]
                  | otherwise = throwError $ EvBuiltInError "Factorial on negative"
mFact _ = infError

mFloor :: [Value] -> Interpreter EvalError Value
mFloor [VNum x, VNum sig] = return . VNum $ sig * intToDbl (floor (x/sig))
mFloor _ = infError

mLog :: [Value] -> Interpreter EvalError Value
mLog [VNum x, VNum b] = return . VNum $ logBase b x
mLog _ = infError

mMod :: [Value] -> Interpreter EvalError Value
mMod [VNum n, VNum d] = return . VNum . intToDbl $ mod (floor n ) (floor d)
mMod _ = infError

mPi :: [Value] -> Interpreter EvalError Value
mPi [] = return . VNum $ pi
mPi _ = infError

mQuot :: [Value] -> Interpreter EvalError Value
mQuot [VNum n, VNum d] = return . VNum . intToDbl $ floor n `quot` floor d
mQuot _ = infError

mRound :: [Value] -> Interpreter EvalError Value
mRound [VNum n, VNum d] = weirdRound round round n d
mRound _ = infError

mRDown :: [Value] -> Interpreter EvalError Value
mRDown [VNum n, VNum d] = weirdRound ceiling floor n d
mRDown _ = infError

mRndUp :: [Value] -> Interpreter EvalError Value
mRndUp [VNum n, VNum d] = weirdRound floor ceiling n d
mRndUp _ = infError

mSqrt :: [Value] -> Interpreter EvalError Value
mSqrt [VNum x] = if x < 0
    then throwError $ EvBuiltInError "Cannot take sqrt of a negative."
    else return . VNum $ sqrt x
mSqrt _ = infError

mSum :: [Value] -> Interpreter EvalError Value
mSum [VNum x, VNum y] = return . VNum $ x + y
mSum _ = infError

mSumSq :: [Value] -> Interpreter EvalError Value
mSumSq [VNum a,VNum b] = return . VNum $ a ** 2 + b ** 2
mSumSq _ = infError

mTrunc :: [Value] -> Interpreter EvalError Value
mTrunc [VNum n, VNum d] = weirdRound round round n d
mTrunc _ = infError

builtInMath :: M.Map String BuiltIn
builtInMath = M.fromList
    [ ("abs",     BuiltIn mAbs   "ABS"     (typeNum :-> typeNum))
    , ("acos",    BuiltIn mAcos  "ACOS"    (typeNum :-> typeNum))
    , ("acosh",   BuiltIn mAcosh "ACOSH"   (typeNum :-> typeNum))
    , ("asin",    BuiltIn mAsin  "ASIN"    (typeNum :-> typeNum))
    , ("asinh",   BuiltIn mAsinh "ASINH"   (typeNum :-> typeNum))
    , ("atan",    BuiltIn mAtan  "ATAN"    (typeNum :-> typeNum))
    , ("atan2",   BuiltIn mAtan2 "ATAN2"   (typeNum :-> typeNum :-> typeNum))
    , ("atanh",   BuiltIn mAtanh "ATANH"   (typeNum :-> typeNum))
    , ("ceiling", BuiltIn mCeil  "CEILING" (typeNum :-> typeNum :-> typeNum))
    , ("combin",  BuiltIn mComb  "COMBIN"  (typeNum :-> typeNum :-> typeNum))
    , ("cos",     BuiltIn mCos   "COS"     (typeNum :-> typeNum))
    , ("cosh",    BuiltIn mCosh  "COSH"    (typeNum :-> typeNum))
    , ("degrees", BuiltIn mDeg   "DEGREES" (typeNum :-> typeNum))
    , ("even",    BuiltIn mEven  "EVEN"    (typeNum :-> typeNum))
    , ("exp",     BuiltIn mExp   "EXP"     (typeNum :-> typeNum))
    , ("fact",    BuiltIn mFact  "FACT"    (typeNum :-> typeNum))
    , ("floor",   BuiltIn mFloor "FLOOR"   (typeNum :-> typeNum :-> typeNum))
    , ("int",     BuiltIn mInt   "INT"     (typeNum :-> typeNum))
    , ("ln",      BuiltIn mLn    "LN"      (typeNum :-> typeNum))
    , ("log",     BuiltIn mLog   "LOG"     (typeNum :-> typeNum :-> typeNum))
    , ("log10",   BuiltIn mLog10 "LOG10"   (typeNum :-> typeNum))
    , ("mod",     BuiltIn mMod   "MOD"     (typeNum :-> typeNum :-> typeNum))
    , ("odd",     BuiltIn mOdd   "ODD"     (typeNum :-> typeNum))
    , ("pi",      BuiltIn mPi    "PI"       typeNum)
    , ("quot",    BuiltIn mQuot "QUOTIENT" (typeNum :-> typeNum :-> typeNum))
    , ("radians", BuiltIn mRad   "RADIANS" (typeNum :-> typeNum))
    , ("round",   BuiltIn mRound "ROUND"   (typeNum :-> typeNum :-> typeNum))
    , ("rounddown",BuiltIn mRDown "ROUNDDOWN" (typeNum :-> typeNum :-> typeNum))
    , ("roundup", BuiltIn mRndUp "ROUNDUP" (typeNum :-> typeNum :-> typeNum))
    , ("sum",     BuiltIn mSum   "SUM"     (typeNum :-> typeNum :-> typeNum))
    , ("sign",    BuiltIn mSign  "SIGN"    (typeNum :-> typeNum))
    , ("sin",     BuiltIn mSin   "SIN"     (typeNum :-> typeNum))
    , ("sinh",    BuiltIn mSinh  "SINH"    (typeNum :-> typeNum))
    , ("sqrt",    BuiltIn mSqrt  "SQRT"    (typeNum :-> typeNum))
    , ("sumsq",   BuiltIn mSumSq "SUMSQ"   (typeNum :-> typeNum :-> typeNum))
    , ("tan",     BuiltIn mTan   "TAN"     (typeNum :-> typeNum))
    , ("tanh",    BuiltIn mTanh  "TANH"    (typeNum :-> typeNum))
    , ("trunc",   BuiltIn mTrunc "TRUNC"   (typeNum :-> typeNum :-> typeNum))
    ]
