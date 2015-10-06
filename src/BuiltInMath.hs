module BuiltInMath (builtInMath) where

import           Control.Monad.Except
import qualified Data.Map as M

import           Type
import           Types

-- TODO:

infError :: Interpreter EvalError Value
infError = error "Received unexpected types. Either the built-in was \
                 \improperly defined or type inference failed."

-- | Run a haskell unary function for a math built-in.
unaryBiHask :: (Double -> Double) -> [Value] -> Interpreter EvalError Value
unaryBiHask f [VNum x] = return . VNum . f $ x
unaryBiHask _ _        = infError

intToDbl :: Integer -> Double
intToDbl = fromIntegral

mathAbs, mathAcos, mathAcosh, mathAsin, mathAsinh, mathAtan, mathAtanh,
    mathCos, mathCosh, degrees, mathEven, mathExp,
    mathInt, mathLn, mathLog10, mathOdd, mathRad, mathSign, mathSin, mathSinh,
    mathTan, mathTanh
     :: [Value] -> Interpreter EvalError Value

mathAbs   = unaryBiHask abs
mathAcos  = unaryBiHask acos
mathAcosh = unaryBiHask acosh
mathAsin  = unaryBiHask asin
mathAsinh = unaryBiHask asinh
mathAtan  = unaryBiHask atan
mathAtanh = unaryBiHask atanh
mathCos   = unaryBiHask cos
mathCosh  = unaryBiHask cosh
degrees   = unaryBiHask (\x -> 180*x/pi)
mathEven  = unaryBiHask (\x -> 2 * intToDbl (round $ x / 2))
mathExp   = unaryBiHask exp
mathInt   = unaryBiHask (\x -> intToDbl $ truncate (if x < 0 then x-1 else x))
mathLn    = unaryBiHask log
mathOdd   = unaryBiHask (\x -> 2 * intToDbl (round $ (x+1.5) / 2) - 1)
mathLog10 = unaryBiHask (logBase 10)
mathRad   = unaryBiHask (\x -> x*pi/180)
mathSign  = unaryBiHask sign
  where
    sign x | x > 0     = 1
           | x < 0     = -1
           | otherwise = 0
mathSin   = unaryBiHask sin
mathSinh  = unaryBiHask sinh
mathTan   = unaryBiHask tan
mathTanh  = unaryBiHask tanh

mathAtan2 :: [Value] -> Interpreter EvalError Value
mathAtan2 [VNum x, VNum y] = return . VNum $ atan2 y x
mathAtan2 _ = infError

mathCeil :: [Value] -> Interpreter EvalError Value
mathCeil [VNum x, VNum sig] = return . VNum $ sig * intToDbl (ceiling (x/sig))
mathCeil _ = infError

mathComb :: [Value] -> Interpreter EvalError Value
mathComb [VNum n, VNum r] = return $ VNum x
  where
    x = intToDbl (fact n) / intToDbl (fact r * fact (n-r))
    fact y = product [1..floor y]
mathComb _ = infError

mathFact :: [Value] -> Interpreter EvalError Value
mathFact [VNum n] | n >= 0    = return . VNum . intToDbl $ product [1..floor n]
                  | otherwise = throwError $ EvBuiltInError "Factorial on negative"
mathFact _ = infError

mathFloor :: [Value] -> Interpreter EvalError Value
mathFloor [VNum x, VNum sig] = return . VNum $ sig * intToDbl (floor (x/sig))
mathFloor _ = infError

mathLog :: [Value] -> Interpreter EvalError Value
mathLog [VNum x, VNum b] = return . VNum $ logBase b x
mathLog _ = infError

mathMod :: [Value] -> Interpreter EvalError Value
mathMod [VNum n, VNum d] = return . VNum . intToDbl $ mod (floor n ) (floor d)
mathMod _ = infError

mathPi :: [Value] -> Interpreter EvalError Value
mathPi [] = return . VNum $ pi
mathPi _ = infError

mathQuot :: [Value] -> Interpreter EvalError Value
mathQuot [VNum n, VNum d] = return . VNum . intToDbl $ floor n `quot` floor d
mathQuot _ = infError

mathRound :: [Value] -> Interpreter EvalError Value
mathRound [VNum n, VNum d] = weirdRound round round n d
mathRound _ = infError

mathRDown :: [Value] -> Interpreter EvalError Value
mathRDown [VNum n, VNum d] = weirdRound ceiling floor n d
mathRDown _ = infError

mathRndUp :: [Value] -> Interpreter EvalError Value
mathRndUp [VNum n, VNum d] = weirdRound floor ceiling n d
mathRndUp _ = infError

mathSqrt :: [Value] -> Interpreter EvalError Value
mathSqrt [VNum x] = if x < 0
    then throwError $ EvBuiltInError "Cannot take sqrt of a negative."
    else return . VNum $ sqrt x
mathSqrt _ = infError

mathSum :: [Value] -> Interpreter EvalError Value
mathSum [VNum x, VNum y] = return . VNum $ x + y
mathSum _ = infError

weirdRound :: (Double -> Integer) -> (Double -> Integer) -> Double -> Double
           -> Interpreter EvalError Value
weirdRound fneg fpos n d
    | d < 0     = return . VNum $ (intToDbl . f $ n / dig) * dig
    | otherwise = return . VNum $ (intToDbl . f $ n * dig) / dig
  where
    dig = 10 ** abs d
    f | n < 0     = fneg
      | otherwise = fpos
mathTrunc :: [Value] -> Interpreter EvalError Value
mathTrunc [VNum n, VNum d] = weirdRound round round n d
mathTrunc _ = infError

builtInMath :: M.Map String BuiltIn
builtInMath = M.fromList
    [ ("abs",     BuiltIn mathAbs   "ABS"    (typeNum :-> typeNum))
    , ("acos",    BuiltIn mathAcos  "ACOS"   (typeNum :-> typeNum))
    , ("acosh",   BuiltIn mathAcosh "ACOSH"  (typeNum :-> typeNum))
    , ("asin",    BuiltIn mathAsin  "ASIN"   (typeNum :-> typeNum))
    , ("asinh",   BuiltIn mathAsinh "ASINH"  (typeNum :-> typeNum))
    , ("atan",    BuiltIn mathAtan  "ATAN"   (typeNum :-> typeNum))
    , ("atan2",   BuiltIn mathAtan2 "ATAN2"  (typeNum :-> typeNum :-> typeNum))
    , ("atanh",   BuiltIn mathAtanh "ATANH"  (typeNum :-> typeNum))
    , ("ceiling", BuiltIn mathCeil  "CEILING" (typeNum :-> typeNum :-> typeNum))
    , ("combin",  BuiltIn mathComb  "COMBIN" (typeNum :-> typeNum :-> typeNum))
    , ("cos",     BuiltIn mathCos   "COS"    (typeNum :-> typeNum))
    , ("cosh",    BuiltIn mathCosh  "COSH"   (typeNum :-> typeNum))
    , ("degrees", BuiltIn degrees   "DEGREES" (typeNum :-> typeNum))
    , ("even",    BuiltIn mathEven  "EVEN"   (typeNum :-> typeNum))
    , ("exp",     BuiltIn mathExp   "EXP"    (typeNum :-> typeNum))
    , ("fact",    BuiltIn mathFact  "FACT"   (typeNum :-> typeNum))
    , ("floor",   BuiltIn mathFloor "FLOOR"  (typeNum :-> typeNum :-> typeNum))
    , ("int",     BuiltIn mathInt   "INT"    (typeNum :-> typeNum))
    , ("ln",      BuiltIn mathLn    "LN"     (typeNum :-> typeNum))
    , ("log",     BuiltIn mathLog   "LOG"    (typeNum :-> typeNum :-> typeNum))
    , ("log10",   BuiltIn mathLog10 "LOG10"  (typeNum :-> typeNum))
    , ("mod",     BuiltIn mathMod   "MOD"    (typeNum :-> typeNum :-> typeNum))
    , ("odd",     BuiltIn mathOdd   "ODD"    (typeNum :-> typeNum))
    , ("pi",      BuiltIn mathPi    "PI"      typeNum)
    , ("quot",    BuiltIn mathQuot  "QUOTIENT" (typeNum :-> typeNum :-> typeNum))
    , ("radians", BuiltIn mathRad   "RADIANS" (typeNum :-> typeNum))
    , ("round",   BuiltIn mathRound "ROUND"  (typeNum :-> typeNum :-> typeNum))
    , ("rounddown",BuiltIn mathRDown "ROUNDDOWN" (typeNum :-> typeNum :-> typeNum))
    , ("roundup", BuiltIn mathRndUp "ROUNDUP" (typeNum :-> typeNum :-> typeNum))
    , ("sum",     BuiltIn mathSum   "SUM"    (typeNum :-> typeNum :-> typeNum))
    , ("sign",    BuiltIn mathSign  "SIGN"   (typeNum :-> typeNum))
    , ("sin",     BuiltIn mathSin   "SIN"    (typeNum :-> typeNum))
    , ("sinh",    BuiltIn mathSinh  "SINH"   (typeNum :-> typeNum))
    , ("sqrt",    BuiltIn mathSqrt  "SQRT"   (typeNum :-> typeNum))
    , ("sumsq",   BuiltIn mSumSq    "SUMSQ"  (typeNum :-> typeNum :-> typeNum))
    , ("tan",     BuiltIn mathTan   "TAN"    (typeNum :-> typeNum))
    , ("tanh",    BuiltIn mathTanh  "TANH"   (typeNum :-> typeNum))
    , ("trunc",   BuiltIn mathTrunc "TRUNC"  (typeNum :-> typeNum :-> typeNum))
    ]
