module BuiltIn.Stats (builtInStats) where

import qualified Data.Map as M
import qualified Data.Vector as V
import           Numeric.SpecFunctions
import           Statistics.Distribution
import           Statistics.Distribution.Binomial
import           Statistics.Distribution.ChiSquared
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.FDistribution
import           Statistics.Distribution.Gamma
import           Statistics.Distribution.Hypergeometric
import           Statistics.Distribution.StudentT
import qualified Statistics.Sample as SS

import           BuiltIn.Common
import           Type
import           Types

-- | Truncate a double back to a double
dTrunc :: Double -> Double
dTrunc x = fromIntegral (truncate x :: Integer)

biAveDev :: BuiltIn
biAveDev = BuiltIn
    { evalVal = aveDev
    , emitVal = const "AVEDEV"
    , typeSig = typeList typeNum :-> typeNum
    , argHelp = "[numbers]"
    , addHelp = Nothing
    }

aveDev :: [Value] -> Interpreter EvalError Value
aveDev [VList xs]
    | length xs > 255 = biErr "Too many arguments (>255)"
    | otherwise = do
        xs' <- vListToDbls xs
        let mn = SS.mean $ V.fromList xs'
        let var = fmap (\x -> abs (mn - x)) xs'
        return . VNum . SS.mean $ V.fromList var
aveDev _ = infError

biAvg :: BuiltIn
biAvg = BuiltIn
    { evalVal = avg
    , emitVal = const "AVERAGE"
    , typeSig = typeList typeNum :-> typeNum
    , argHelp = "[numbers]"
    , addHelp = Nothing
    }

avg :: [Value] -> Interpreter EvalError Value
avg [VList xs]
    | length xs > 255 = biErr "Too many arguments (>255)"
    | otherwise = do
        xs' <- vListToDbls xs
        return . VNum . SS.mean $ V.fromList xs'
avg _ = infError

biBinom :: BuiltIn
biBinom = BuiltIn
    { evalVal = binom
    , emitVal = const "BINOMDIST"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeBool :-> typeNum
    , argHelp = "number_s trials probability_s cumulative"
    , addHelp = Nothing
    }

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

biChiDist :: BuiltIn
biChiDist = BuiltIn
    { evalVal = chiDist
    , emitVal = const "CHIDIST"
    , typeSig = typeNum :-> typeNum :-> typeNum
    , argHelp = "x degrees_freedom"
    , addHelp = Nothing
    }

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
biChiInv :: BuiltIn
biChiInv = BuiltIn
    { evalVal = chiInv
    , emitVal = const "CHIINV""
    , typeSig = typeNum :-> typeNum :-> typeNum
    , argHelp = "probability degrees_freedom"
    , addHelp = Nothing
    }

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

biConfd :: BuiltIn
biConfd = BuiltIn
    { evalVal = confd
    , emitVal = const "CONFIDENCE"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "alpha standard_dev size"
    , addHelp = Nothing
    }

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

biExpDist :: BuiltIn
biExpDist = BuiltIn
    { evalVal = expDist
    , emitVal = const "EXPONDIST"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "x lambda cumulative"
    , addHelp = Nothing
    }

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

biFDist :: BuiltIn
biFDist = BuiltIn
    { evalVal = fDist
    , emitVal = const "FDIST"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "x degrees_freedom1 degrees_freedom2"
    , addHelp = Nothing
    }

fDist :: [Value] -> Interpreter EvalError Value
fDist [VNum x, VNum df1, VNum df2]
    | x < 0 = biErr "First argument to fdist must be positive"
    | df1 < 1 || df1 >= 10**10 = biErr "fdist numerator degrees of freedom out of range"
    | df2 < 1 || df2 >= 10**10 = biErr "fdist denominator degrees of freedom out of range"
    | otherwise = return . VNum $ complCumulative dist x
  where
    dist = fDistribution (truncate df1) (truncate df2)
fDist _ = infError

biFisher :: BuiltIn
biFisher = BuiltIn
    { evalVal = fisher
    , emitVal = const "FISHER"
    , typeSig = typeNum :-> typeNum
    , argHelp = "x"
    , addHelp = Nothing
    }

fisher :: [Value] -> Interpreter EvalError Value
fisher [VNum x]
    | x >= 1 || x <= -1 = biErr "fisher requires an argument between -1 and 1"
    | otherwise = return . VNum $ atanh x
fisher _ = infError

biFisherInv :: BuiltIn
biFisherInv = BuiltIn
    { evalVal = fisherinv
    , emitVal = const "FISHERINV"
    , typeSig = typeNum :-> typeNum
    , argHelp = "y"
    , addHelp = Nothing
    }

fisherinv :: [Value] -> Interpreter EvalError Value
fisherinv [VNum x] = return . VNum $ tanh x
fisherinv _ = infError

biGamDist :: BuiltIn
biGamDist = BuiltIn
    { evalVal = gamDist
    , emitVal = const "GAMMADIST"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeBool :-> typeNum
    , argHelp = "x alpha beta cumulative"
    , addHelp = Nothing
    }

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

biGamInv :: BuiltIn
biGamInv = BuiltIn
    { evalVal = gamInv
    , emitVal = const "GAMMAINV"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "probability alpha beta"
    , addHelp = Nothing
    }

gamInv :: [Value] -> Interpreter EvalError Value
gamInv [VNum p, VNum alph, VNum beta]
    | p < 0 || p > 1 = biErr "gammainv requires probability to be >= 0 and <= 1"
    | alph <= 0 = biErr "gammainv requires alpha > 0"
    | beta <= 0 = biErr "gammainv requires beta > 0"
    | otherwise = return . VNum $ quantile dist p
  where
    dist = gammaDistr alph beta
gamInv _ = infError

biGamLn :: BuiltIn
biGamLn = BuiltIn
    { evalVal = gamLn
    , emitVal = const "GAMMALN"
    , typeSig = typeNum :-> typeNum
    , argHelp = "x"
    , addHelp = Nothing
    }

gamLn :: [Value] -> Interpreter EvalError Value
gamLn [VNum x]
    | x <= 0 = biErr "gammaln requires its argument be > 0"
    | otherwise = return . VNum $ logGammaL x
gamLn _ = infError

biHGeoDist :: BuiltIn
biHGeoDist = BuiltIn
    { evalVal = hgeoDist
    , emitVal = const "HYPGEOMDIST"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "sample_s number_sample population_s number_population"
    , addHelp = Nothing
    }

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
    [ ("avedev",      biAveDev)
    , ("average",     biAvg)
    , ("binomdist",   biBinom)
    , ("chidist",     biChiDist)
--  , ("chiinv",      biChiInv)
    , ("confidence",  biConfd)
    , ("expondist",   biExpDist)
    , ("fdist",       biFDist)
    , ("fisher",      biFisher)
    , ("fisherinv",   biFisherInv)
    , ("gammadist",   biGamDist)
    , ("gammainv",    biGamInv)
    , ("gammaln",     biGamLn)
    , ("hypgeomdist", biHGeoDist)
    ]
