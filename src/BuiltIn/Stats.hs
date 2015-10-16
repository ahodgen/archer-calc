module BuiltIn.Stats (builtInStats) where

import           Control.Monad.Except
import qualified Data.Map as M
import           Numeric.SpecFunctions
import           Statistics.Distribution
import           Statistics.Distribution.Binomial
import           Statistics.Distribution.ChiSquared
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.FDistribution
import           Statistics.Distribution.Gamma
import           Statistics.Distribution.Hypergeometric
import           Statistics.Distribution.StudentT

import           Type
import           Types

infError :: Interpreter EvalError Value
infError = error "Received unexpected types. Either the built-in was \
                 \improperly defined or type inference failed."

biErr :: String -> Interpreter EvalError Value
biErr = throwError . EvBuiltInError

-- | Truncate a double back to a double
dTrunc :: Double -> Double
dTrunc x = fromIntegral (truncate x :: Integer)

binom :: [Value] -> Interpreter EvalError Value
binom [VNum num, VNum trls, VNum prob, VBool cum]
    | prob < 0 || prob > 1 = biErr "Probability in binomdist must be between 0 and 1"
    | num  < 0 = biErr "Number of successes in binomdist cannot be negative"
    | num > trls = biErr "Successes > trials is not allowed in binomdist"
    | otherwise = if cum
        then return . VNum $ cumulative  dist (dTrunc num)
        else return . VNum $ probability dist (truncate num)
  where
    dist   = binomial (truncate trls) prob
binom _         = infError

chiDist :: [Value] -> Interpreter EvalError Value
chiDist [VNum x, VNum df]
    | x < 0 = biErr "First argument to chidist must be a positive number"
    | df < 1  = biErr "Degrees of freedom for chidist must be 1 or greater"
    | df > 10**10 = biErr "Degrees of freedom for chidist too large"
    | otherwise = return . VNum $ complCumulative dist x
  where
    dist = chiSquared (truncate df)
chiDist _ = infError

{-
chiInv :: [Value] -> Interpreter EvalError Value
chiInv [VNum prob, VNum df]
    | prob < 0 || prob > 1 = biErr "Probability in chiinv must be between 0 and 1"
    | df < 1  = biErr "Degrees of freedom for chiinv must be 1 or greater"
    | df > 10**10 = biErr "Degrees of freedom for chiinv too large"
    | otherwise = return . VNum $ cumulative dist prob
  where
    dist = chiSquared (truncate df)
chiInv _ = infError
-}

confd :: [Value] -> Interpreter EvalError Value
confd [VNum alph, VNum std, VNum size]
    | alph <= 0 || alph >= 1 = biErr "Alpha for confidence must be between 0 and 1"
    | size < 1 = biErr "Sample size in confidence must be greater than one"
    | std <= 0 = biErr "Standard deviation for confidence must be >= 0"
    | otherwise = return $ VNum $ cv * se
  where
    se = std / sqrt (dTrunc size)
    df = size - 1
    cp = 1 - alph/2
    cv = quantile (studentT df) cp
confd _ = infError

expDist :: [Value] -> Interpreter EvalError Value
expDist [VNum x, VNum lam, VBool cum]
    | x < 0 = biErr "First paramter to expondist must be >= 0"
    | lam <= 0 = biErr "Lambda must be > 0 in expondist"
    | otherwise = if cum
        then return . VNum $ cumulative dist x
        else return . VNum $ density dist x
  where
    dist = exponential lam
expDist _ = infError

fDist :: [Value] -> Interpreter EvalError Value
fDist [VNum x, VNum df1, VNum df2]
    | x < 0 = biErr "First argument to fdist must be positive"
    | df1 < 1 || df1 >= 10**10 = biErr "fdist numerator degrees of freedom out of range"
    | df2 < 1 || df2 >= 10**10 = biErr "fdist denominator degrees of freedom out of range"
    | otherwise = return . VNum $ complCumulative dist x
  where
    dist = fDistribution (truncate df1) (truncate df2)
fDist _ = infError

fisher :: [Value] -> Interpreter EvalError Value
fisher [VNum x]
    | x >= 1 || x <= -1 = biErr "fisher requires an argument between -1 and 1"
    | otherwise = return . VNum $ atanh x
fisher _ = infError

fisherinv :: [Value] -> Interpreter EvalError Value
fisherinv [VNum x] = return . VNum $ tanh x
fisherinv _ = infError

gamDist :: [Value] -> Interpreter EvalError Value
gamDist [VNum x, VNum alph, VNum beta, VBool cum]
    | x < 0 = biErr "gamma requires its first argument to be positive"
    | alph <= 0 = biErr "gammadist requires alpha > 0"
    | beta <= 0 = biErr "gammadist requires beta > 0"
    | otherwise = if cum
        then return . VNum $ cumulative dist x
        else return . VNum $ density dist x
  where
    dist = gammaDistr alph beta
gamDist _ = infError

gamInv :: [Value] -> Interpreter EvalError Value
gamInv [VNum p, VNum alph, VNum beta]
    | p < 0 || p > 1 = biErr "gammainv requires probability to be >= 0 and <= 1"
    | alph <= 0 = biErr "gammainv requires alpha > 0"
    | beta <= 0 = biErr "gammainv requires beta > 0"
    | otherwise = return . VNum $ quantile dist p
  where
    dist = gammaDistr alph beta
gamInv _ = infError

gamLn :: [Value] -> Interpreter EvalError Value
gamLn [VNum x]
    | x <= 0 = biErr "gammaln requires its argument be > 0"
    | otherwise = return . VNum $ logGammaL x
gamLn _ = infError

hgeoDist :: [Value] -> Interpreter EvalError Value
hgeoDist [VNum sam, VNum num, VNum pop, VNum psiz]
    | sam < 0 = biErr "hypgeomdist sample_s must be >=0"
    | sam > min num pop = biErr "hypgeomdist sample_s too large (> number_sample or population_s)"
    | num <= 0 = biErr "hypgeomdist number_sample must be > 0"
    | num > psiz = biErr "hypgeomdist number_sample too large (> number_population)"
    | pop <= 0 = biErr "hypgeomdist population_s must be > 0"
    | pop > psiz = biErr "hypgeomdist population_s too large (> number_population)"
    | psiz <= 0 = biErr "hypgeomdist number_population must be > 0"
    | otherwise = return . VNum $ probability dist $ truncate sam
  where
    dist = hypergeometric (truncate pop) (truncate psiz) (truncate num) -- psiz
hgeoDist _ = infError

builtInStats :: M.Map String BuiltIn
builtInStats = M.fromList
    [ ("binomdist", BuiltIn binom "BINOMDIST"   (typeNum :-> typeNum :-> typeNum :-> typeBool :-> typeNum))
    , ("chidist",   BuiltIn chiDist "CHIDIST" (typeNum :-> typeNum :-> typeNum))
--    , ("chiinv",    BuiltIn chiInv  "CHIINV"  (typeNum :-> typeNum :-> typeNum))
    , ("confidence", BuiltIn confd "CONFIDENCE" (typeNum :-> typeNum :-> typeNum :-> typeNum))
    , ("expondist",  BuiltIn expDist "EXPONDIST" (typeNum :-> typeNum :-> typeNum :-> typeNum))
    , ("fdist",  BuiltIn fDist "FDIST" (typeNum :-> typeNum :-> typeNum :-> typeNum))
    , ("fisher", BuiltIn fisher "FISHER" (typeNum :-> typeNum))
    , ("fisherinv", BuiltIn fisherinv "FISHERINV" (typeNum :-> typeNum))
    , ("gammadist", BuiltIn gamDist "GAMMADIST" (typeNum :-> typeNum :-> typeNum :-> typeBool :-> typeNum))
    , ("gammainv",  BuiltIn gamInv  "GAMMAINV"  (typeNum :-> typeNum :-> typeNum :-> typeNum))
    , ("gammaln",   BuiltIn gamLn   "GAMMALN"   (typeNum :-> typeNum))
    , ("hypgeomdist", BuiltIn hgeoDist "HYPGEOMDIST" (typeNum :-> typeNum :-> typeNum :-> typeNum :-> typeNum))
    ]
