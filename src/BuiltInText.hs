module BuiltInText (builtInText) where

import           Control.Monad.Except
import           Data.Char (toLower, toUpper, isAlpha)
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Monoid ((<>))

import           Type
import           Types

-- TODO:
-- MASKEDTEXT
-- NUMBERFORMAT

infError :: Interpreter EvalError Value
infError = error "Received unexpected types. Either the built-in was \
                 \improperly defined or type inference failed."

biErr :: String -> Interpreter EvalError Value
biErr = throwError . EvBuiltInError

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

left :: [Value] -> Interpreter EvalError Value
left [VText xs, VNum cnt]
    | cnt < 0   = biErr "'left' cannot take a negative number."
    | otherwise = return . VText $ take (floor cnt) xs
left _ = infError

lenTxt :: [Value] -> Interpreter EvalError Value
lenTxt [VText xs] = return . VNum . fromIntegral $ length xs
lenTxt _ = infError

lower :: [Value] -> Interpreter EvalError Value
lower [VText xs] = return . VText $ fmap toLower xs
lower _ = infError

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

right :: [Value] -> Interpreter EvalError Value
right [VText xs, VNum cnt]
    | cnt < 0   = biErr "'right' cannot take a negative number."
    | otherwise = return . VText . reverse . take (floor cnt) $ reverse xs
right _ = infError

substring :: [Value] -> Interpreter EvalError Value
substring [VText xs, VNum st, VNum cnt]
    | st < 1 = biErr "'substring' start cannot be less than 1."
    | floor st > length xs = biErr "'substring' start is greater than length."
    | otherwise = return . VText . take (floor cnt) . drop (floor st - 1) $ xs
substring _ = infError

trim :: [Value] -> Interpreter EvalError Value
trim [VText xs] = return . VText . unwords . words $ xs
trim _ = infError

upper :: [Value] -> Interpreter EvalError Value
upper [VText xs] = return . VText $ fmap toUpper xs
upper _ = infError

builtInText :: M.Map String BuiltIn
builtInText = M.fromList
    [ ("find",     BuiltIn findTxt   "FIND"   (typeText :-> typeText :-> typeNum :-> typeNum))
    , ("left",     BuiltIn left      "LEFT"   (typeText :-> typeNum :-> typeText))
    , ("len",      BuiltIn lenTxt    "LEN"    (typeText :-> typeNum))
    , ("lower",    BuiltIn lower     "LOWER"  (typeText :-> typeText))
    , ("proper",   BuiltIn proper    "PROPER" (typeText :-> typeText))
    , ("right",    BuiltIn right     "RIGHT"  (typeText :-> typeNum :-> typeText))
    , ("substring",BuiltIn substring "SUBSTRING" (typeText :-> typeNum :-> typeNum :-> typeText))
    , ("trim",     BuiltIn trim      "TRIM"   (typeText :-> typeText))
    , ("upper",    BuiltIn upper     "UPPER"  (typeText :-> typeText))
    ]
