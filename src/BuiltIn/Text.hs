module BuiltIn.Text (builtInText) where

import           Data.Char (toLower, toUpper, isAlpha)
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Monoid ((<>))

import           BuiltIn.Common
import           Type
import           Types

-- TODO:
-- MASKEDTEXT
-- NUMBERFORMAT

biFindTxt :: BuiltIn
biFindTxt = BuiltIn
    { evalVal = findTxt
    , emitVal = const "FIND"
    , typeSig = typeText :-> typeText :-> typeNum :-> typeNum
    , argHelp = "find_text field_ref start_num"
    , addHelp = Nothing
    }

-- XXX: What is the failure case?
findTxt :: [Value] -> Interpreter EvalError Value
findTxt [VText ndl, VText hystk, VNum st] =
    return . VNum . fromIntegral . (+1) . fnd rst $ drop rst hystk
  where
    rst = floor st
    fnd n [] = n
    fnd n xs = if ndl `isPrefixOf` xs
        then n
        else fnd (n+1) (drop 1 xs)
findTxt _ = infError

biLeft :: BuiltIn
biLeft = BuiltIn
    { evalVal = left
    , emitVal = const "LEFT"
    , typeSig = typeText :-> typeNum :-> typeText
    , argHelp = "text num_chars"
    , addHelp = Nothing
    }

left :: [Value] -> Interpreter EvalError Value
left [VText xs, VNum cnt]
    | cnt < 0   = biErr "'left' cannot take a negative number."
    | otherwise = return . VText $ take (floor cnt) xs
left _ = infError

biLenTxt :: BuiltIn
biLenTxt = BuiltIn
    { evalVal = lenTxt
    , emitVal = const "LEN"
    , typeSig = typeText :-> typeNum
    , argHelp = "text"
    , addHelp = Nothing
    }

lenTxt :: [Value] -> Interpreter EvalError Value
lenTxt [VText xs] = return . VNum . fromIntegral $ length xs
lenTxt _ = infError

biLower :: BuiltIn
biLower = BuiltIn
    { evalVal = lower
    , emitVal = const "LOWER"
    , typeSig = typeText :-> typeText
    , argHelp = "text"
    , addHelp = Nothing
    }

lower :: [Value] -> Interpreter EvalError Value
lower [VText xs] = return . VText $ fmap toLower xs
lower _ = infError

biProper :: BuiltIn
biProper = BuiltIn
    { evalVal = proper
    , emitVal = const "PROPER"
    , typeSig = typeText :-> typeText
    , argHelp = "text"
    , addHelp = Nothing
    }

proper :: [Value] -> Interpreter EvalError Value
proper [VText ts] = return . VText $ go ts
  where
    go [] = []
    go xs = prp mat <> take 1 rest <> go (drop 1 rest)
      where
        (mat,rest) = span isAlpha xs
    prp [] = []
    prp (x:xs) = toUpper x : fmap toLower xs
proper _ = infError

biRight :: BuiltIn
biRight = BuiltIn
    { evalVal = right
    , emitVal = const "RIGHT"
    , typeSig = typeText :-> typeNum :-> typeText
    , argHelp = "text num_chars"
    , addHelp = Nothing
    }

right :: [Value] -> Interpreter EvalError Value
right [VText xs, VNum cnt]
    | cnt < 0   = biErr "'right' cannot take a negative number."
    | otherwise = return . VText . reverse . take (floor cnt) $ reverse xs
right _ = infError

biSubstring :: BuiltIn
biSubstring = BuiltIn
    { evalVal = substring
    , emitVal = const "SUBSTRING"
    , typeSig = typeText :-> typeNum :-> typeNum :-> typeText
    , argHelp = "text start_num num_chars"
    , addHelp = Nothing
    }

substring :: [Value] -> Interpreter EvalError Value
substring [VText xs, VNum st, VNum cnt]
    | st < 1 = biErr "'substring' start cannot be less than 1."
    | floor st > length xs = biErr "'substring' start is greater than length."
    | otherwise = return . VText . take (floor cnt) . drop (floor st - 1) $ xs
substring _ = infError

biTrim :: BuiltIn
biTrim = BuiltIn
    { evalVal = trim
    , emitVal = const "TRIM"
    , typeSig = typeText :-> typeNum
    , argHelp = "text"
    , addHelp = Nothing
    }

trim :: [Value] -> Interpreter EvalError Value
trim [VText xs] = return . VText . unwords . words $ xs
trim _ = infError

biUpper :: BuiltIn
biUpper= BuiltIn
    { evalVal = upper
    , emitVal = const "UPPER"
    , typeSig = typeText :-> typeNum
    , argHelp = "text"
    , addHelp = Nothing
    }

upper :: [Value] -> Interpreter EvalError Value
upper [VText xs] = return . VText $ fmap toUpper xs
upper _ = infError

builtInText :: M.Map String BuiltIn
builtInText = M.fromList
    [ ("find",      biFindTxt)
    , ("left",      biLeft)
    , ("len",       biLenTxt)
    , ("lower",     biLower)
    , ("proper",    biProper)
    , ("right",     biRight)
    , ("substring", biSubstring)
    , ("trim",      biTrim)
    , ("upper",     biUpper)
    ]
