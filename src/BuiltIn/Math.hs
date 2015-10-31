module BuiltIn.Math (builtInMath) where

import qualified Data.Map as M

import           BuiltIn.Common
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
                  | otherwise = biErr "Factorial on negative"
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
    then biErr "Cannot take sqrt of a negative."
    else return . VNum $ sqrt x
mSqrt _ = infError

mSum :: [Value] -> Interpreter EvalError Value
mSum [VList xs]
    | length xs > 255 = biErr "Too many arguments (>255)"
    | otherwise = do
        xs' <- vListToDbls xs
        return . VNum . sum $ xs'
mSum _ = infError

mSumSq :: [Value] -> Interpreter EvalError Value
mSumSq [VNum a,VNum b] = return . VNum $ a ** 2 + b ** 2
mSumSq _ = infError

mTrunc :: [Value] -> Interpreter EvalError Value
mTrunc [VNum n, VNum d] = weirdRound round round n d
mTrunc _ = infError

builtInMath :: M.Map String BuiltIn
builtInMath = M.fromList
    [ ("abs",     BuiltIn mAbs   (const "ABS")    (typeNum :-> typeNum) "number" Nothing)
    , ("acos",    BuiltIn mAcos  (const "ACOS")   (typeNum :-> typeNum) "number" Nothing)
    , ("acosh",   BuiltIn mAcosh (const "ACOSH")  (typeNum :-> typeNum) "number" Nothing)
    , ("asin",    BuiltIn mAsin  (const "ASIN")   (typeNum :-> typeNum) "number" Nothing)
    , ("asinh",   BuiltIn mAsinh (const "ASINH")  (typeNum :-> typeNum) "number" Nothing)
    , ("atan",    BuiltIn mAtan  (const "ATAN")   (typeNum :-> typeNum) "number" Nothing)
    , ("atan2",   BuiltIn mAtan2 (const "ATAN2")  (typeNum :-> typeNum :-> typeNum) "x_number y_number" Nothing)
    , ("atanh",   BuiltIn mAtanh (const "ATANH")  (typeNum :-> typeNum) "number" Nothing)
    , ("ceiling", BuiltIn mCeil  (const "CEILING") (typeNum :-> typeNum :-> typeNum) "number significance" Nothing)
    , ("combin",  BuiltIn mComb  (const "COMBIN") (typeNum :-> typeNum :-> typeNum) "number number_chosen" Nothing)
    , ("cos",     BuiltIn mCos   (const "COS")    (typeNum :-> typeNum) "radians" Nothing)
    , ("cosh",    BuiltIn mCosh  (const "COSH")   (typeNum :-> typeNum) "number" Nothing)
    , ("degrees", BuiltIn mDeg   (const "DEGREES")(typeNum :-> typeNum) "radians" Nothing)
    , ("even",    BuiltIn mEven  (const "EVEN")   (typeNum :-> typeNum) "number" Nothing)
    , ("exp",     BuiltIn mExp   (const "EXP")    (typeNum :-> typeNum) "number" Nothing)
    , ("fact",    BuiltIn mFact  (const "FACT")   (typeNum :-> typeNum) "number" Nothing)
    , ("floor",   BuiltIn mFloor (const "FLOOR")  (typeNum :-> typeNum :-> typeNum) "number significance" Nothing)
    , ("int",     BuiltIn mInt   (const "INT")    (typeNum :-> typeNum) "number" Nothing)
    , ("ln",      BuiltIn mLn    (const "LN")     (typeNum :-> typeNum) "number" Nothing)
    , ("log",     BuiltIn mLog   (const "LOG")    (typeNum :-> typeNum :-> typeNum) "number base" Nothing)
    , ("log10",   BuiltIn mLog10 (const "LOG10")  (typeNum :-> typeNum) "number" Nothing)
    , ("mod",     BuiltIn mMod   (const "MOD")    (typeNum :-> typeNum :-> typeNum) "number divisor" Nothing)
    , ("odd",     BuiltIn mOdd   (const "ODD")    (typeNum :-> typeNum) "number" Nothing)
    , ("pi",      BuiltIn mPi    (const "PI")      typeNum "" Nothing)
    , ("quot",    BuiltIn mQuot (const "QUOTIENT") (typeNum :-> typeNum :-> typeNum) "numerator denominator" Nothing)
    , ("radians", BuiltIn mRad   (const "RADIANS") (typeNum :-> typeNum) "degrees" Nothing)
    , ("round",   BuiltIn mRound (const "ROUND")  (typeNum :-> typeNum :-> typeNum) "number num_digits" Nothing)
    , ("rounddown",BuiltIn mRDown (const "ROUNDDOWN") (typeNum :-> typeNum :-> typeNum) "number num_digits" Nothing)
    , ("roundup", BuiltIn mRndUp (const "ROUNDUP") (typeNum :-> typeNum :-> typeNum) "number num_digits" Nothing)
    , ("sum",     BuiltIn mSum (const "SUM")    (typeList typeNum :-> typeNum) "[numbers]" Nothing)
    , ("sign",    BuiltIn mSign  (const "SIGN")   (typeNum :-> typeNum) "number" Nothing)
    , ("sin",     BuiltIn mSin   (const "SIN")    (typeNum :-> typeNum) "radians" Nothing)
    , ("sinh",    BuiltIn mSinh  (const "SINH")   (typeNum :-> typeNum) "number" Nothing)
    , ("sqrt",    BuiltIn mSqrt  (const "SQRT")   (typeNum :-> typeNum) "number" Nothing)
    , ("sumsq",   BuiltIn mSumSq (const "SUMSQ")  (typeNum :-> typeNum :-> typeNum) "number1 number2" Nothing)
    , ("tan",     BuiltIn mTan   (const "TAN")    (typeNum :-> typeNum) "radians" Nothing)
    , ("tanh",    BuiltIn mTanh  (const "TANH")   (typeNum :-> typeNum) "number" Nothing)
    , ("trunc",   BuiltIn mTrunc (const "TRUNC")  (typeNum :-> typeNum :-> typeNum) "number num_digits" Nothing)
    ]
