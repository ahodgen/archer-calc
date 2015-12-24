module BuiltIn.Finance (builtInFinance) where

import qualified Data.Map as M

import           BuiltIn.Common
import           Type
import           Types

retDoll :: Double -> Interpreter EvalError Value
retDoll x = return . VNum $ fromIntegral ((round $ 100 * x) :: Integer) / 100

biDb :: BuiltIn
biDb = BuiltIn
    { evalVal = db
    , emitVal = const "DB"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "cost salvage life period month"
    , addHelp = Nothing
    }

db :: [Value] -> Interpreter EvalError Value
db [VNum cost, VNum svg, VNum lif, VNum per, VNum mon] =
    retDoll . dif $ drop (truncate per - 1) allp
  where
    dif (a:b:_) = b - a
    dif _ = error "Whoa"
    allp = 0 : scanl pdep st [1..lif]
    st = cost * drate * mon/12
    drate = round3 $ 1 - ((svg/cost)**(1/lif))
    round3 x = fromIntegral (round (x * 1000) :: Integer) / 1000
    pdep b x
        | x == lif = ((cost - b) * drate * (12 - mon)) / 12 + b
        | otherwise = (cost - b) * drate + b
db _ = infError

biDdb :: BuiltIn
biDdb = BuiltIn
    { evalVal = ddb
    , emitVal = const "DDB"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "cost salvage life period factor"
    , addHelp = Nothing
    }

ddb :: [Value] -> Interpreter EvalError Value
ddb [VNum cost, VNum sv, VNum lif, VNum per, VNum fac] =
    retDoll . dif $ drop (truncate per - 1) allp
  where
    dif (a:b:_) = b - a
    dif _ = error "Whoa"
    allp = scanl pdep 0 [1..lif]
    dr = 1/lif * fac
    pdep b _ = if cost - mdep b < sv
                then cost - sv
                else mdep b
    mdep x = dr * (cost - x) + x
ddb _ = infError

biNpv :: BuiltIn
biNpv = BuiltIn
    { evalVal = npv
    , emitVal = const "NPV"
    , typeSig = typeNum :-> typeList typeNum :-> typeNum
    , argHelp = "rate [cash_flows]"
    , addHelp = Nothing
    }

npv :: [Value] -> Interpreter EvalError Value
npv [VNum rate, VList xs]
    | length xs > 255 = biErr "Too many arguments (>255)"
    | otherwise = do
        xs' <- vListToDbls xs
        return . VNum . sum $ zipWith calcPer xs' [1..]
  where
    calcPer cf p = cf / (1 + rate)**p
npv _ = infError

biSln :: BuiltIn
biSln = BuiltIn
    { evalVal = sln
    , emitVal = const "SLN"
    , typeSig = typeNum :-> typeNum :-> typeNum :-> typeNum
    , argHelp = "cost salvage life"
    , addHelp = Nothing
    }
sln :: [Value] -> Interpreter EvalError Value
sln [VNum cost, VNum svg, VNum life] =
    return $ VNum $ (cost - svg) / life
sln _ = infError

builtInFinance :: M.Map String BuiltIn
builtInFinance = M.fromList
    [ ("db",  biDb)
    , ("ddb", biDdb)
    , ("npv", biNpv)
    , ("sln", biSln)
    ]
