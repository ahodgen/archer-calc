{-# LANGUAGE OverloadedStrings #-}

-- This module runs the examples from the
-- "RSA Archer GRC Platform Help Center" as tests.

-- import           Debug.Trace
import           Test.Hspec

import           Pretty()

import           TestDate
import           TestMath
import           TestText

main :: IO ()
main = hspec $ do
    testBuiltInDate
    testBuiltInText
    testBuiltInMath
