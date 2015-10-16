{-# LANGUAGE OverloadedStrings #-}
module TestLogic where

import           Test.Hspec

import           Pretty()
import           Types

import           TestCommon

testBuiltInLogic :: Spec
testBuiltInLogic =
    describe "Built-in Logic Functions" $
      describe "NOT" $ do
        checkBuiltIn BiCheck
            { emit  = "NOT([Rating]=10)"
            , value = Right $ VBool False
            , expr  = "not (rt == 10);"
            , defs  = "let rt = field \"Rating\" : Num as 10;"
            }
        checkBuiltIn BiCheck
            { emit  = "NOT([Number of Clients in Attendance]>20)"
            , value = Right $ VBool True
            , expr  = "not (nca>20)"
            , defs  = "let nca = field \"Number of Clients in Attendance\" : Num as 12;"
            }
