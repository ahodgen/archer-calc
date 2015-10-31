{-# LANGUAGE OverloadedStrings #-}
module TestMath where

import           Test.Hspec
import qualified Data.Text.Lazy as L

import           TestCommon

import           Pretty()
import           Types

testBuiltInMath :: Spec
testBuiltInMath =
    describe "Built-in Math Functions" $ do
      describe "ABS" $ do
        checkBuiltIn BiCheck
            { emit  = "ABS(-8)"
            , value = Right $ VNum 8
            , expr  = "abs (-8);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ABS([Yearly Profit])"
            , value = Right $ VNum 1234
            , expr  = "abs yp;"
            , defs = "let yp = field \"Yearly Profit\" : Num as -1234;"
            }
      describe "ACOS" $ do
        checkBuiltIn BiCheck
            { emit  = "ACOS(0.5)"
            , value = Right $ VNum 1.047198
            , expr  = "acos 0.5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ACOS([Angle Cosine])"
            , value = Right $ VNum 0.785398
            , expr  = "acos ac;"
            , defs  = "let ac = field \"Angle Cosine\" : Num as 0.707107;"
            }
      describe "ACOSH" $ do
        checkBuiltIn BiCheck
            { emit  = "ACOSH(1)"
            , value = Right $ VNum 0
            , expr  = "acosh 1;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ACOSH([Number])"
            , value = Right $ VNum 2.292432
            , expr  = "acosh nm;"
            , defs  = "let nm = field \"Number\" : Num as 5;"
            }
      describe "ASIN" $ do
        checkBuiltIn BiCheck
            { emit  = "ASIN(0.5)"
            , value = Right $ VNum 0.523599
            , expr  = "asin 0.5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ASIN([Angle Sine])"
            , value = Right $ VNum 1.570796
            , expr  = "asin as;"
            , defs  = "let as = field \"Angle Sine\" : Num as 1;"
            }
      describe "ASINH" $ do
        checkBuiltIn BiCheck
            { emit  = "ASINH(1)"
            , value = Right $ VNum 0.881374
            , expr  = "asinh 1;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ASINH([Number])"
            , value = Right $ VNum 2.312438
            , expr  = "asinh nm;"
            , defs  = "let nm = field \"Number\" : Num as 5;"
            }
      describe "ATAN" $ do
        checkBuiltIn BiCheck
            { emit  = "ATAN(0.5)"
            , value = Right $ VNum 0.463648
            , expr  = "atan 0.5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ATAN([Angle Tangent])"
            , value = Right $ VNum 0.785398
            , expr  = "atan at;"
            , defs  = "let at = field \"Angle Tangent\" : Num as 1;"
            }
      describe "ATAN2" $ do
        checkBuiltIn BiCheck
            { emit  = "ATAN2(2,2)"
            , value = Right $ VNum 0.785398
            , expr  = "atan2 2 2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ATAN2([X Point],[Y Point])"
            , value = Right $ VNum 1.373401
            , expr  = "atan2 x y;"
            , defs  = "let x = field \"X Point\" : Num as 1;\
                      \let y = field \"Y Point\" : Num as 5;"
            }
      describe "ATANH" $ do
        checkBuiltIn BiCheck
            { emit  = "ATANH(0.5)"
            , value = Right $ VNum 0.549306
            , expr  = "atanh 0.5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ATANH([Number])"
            , value = Right $ VNum (-0.25541)
            , expr  = "atanh nm;"
            , defs  = "let nm = field \"Number\" : Num as (-0.25);"
            }
      describe "CEILING" $ do
        checkBuiltIn BiCheck
            { emit  = "CEILING([Score],1)"
            , value = Right $ VNum 3
            , expr  = "ceiling sc 1;"
            , defs  = "let sc = field \"Score\" : Num as 2.5;"
            }
        checkBuiltIn BiCheck
            { emit  = "CEILING(SUM([Risk],[Criticality]),5)"
            , value = Right $ VNum 20
            , expr  = "ceiling (sum [risk,crit]) 5"
            , defs  = "let risk = field \"Risk\" : Num as 8;\
                      \let crit = field \"Criticality\" : Num as 9.1;"
            }
      describe "COMBIN" $
        checkBuiltIn BiCheck
            { emit  = "COMBIN([Candidates],[Team Size])"
            , value = Right $ VNum 28
            , expr  = "combin cand ts;"
            , defs  = "let cand = field \"Candidates\" : Num as 8;\
                      \let ts = field \"Team Size\" : Num as 2;"
            }
      describe "COS" $ do
        checkBuiltIn BiCheck
            { emit  = "COS(1.047)"
            , value = Right $ VNum 0.500171
            , expr  = "cos 1.047;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "COS(60*PI()/180)"
            , value = Right $ VNum 0.5
            , expr  = "cos (60*pi/180);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "COS(RADIANS(60))"
            , value = Right $ VNum 0.5
            , expr  = "cos (radians 60);"
            , defs  = L.empty
            }
      describe "COSH" $ do
        checkBuiltIn BiCheck
            { emit  = "COSH(4)"
            , value = Right $ VNum 27.30823
            , expr  = "cosh 4;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "COSH(EXP(1))"
            , value = Right $ VNum 7.610125
            , expr  = "cosh (exp 1);"
            , defs  = L.empty
            }
      describe "DEGREES" $
        checkBuiltIn BiCheck
            { emit  = "DEGREES(PI())"
            , value = Right $ VNum 180
            , expr  = "degrees pi;"
            , defs  = L.empty
            }
      describe "EVEN" $ do
        checkBuiltIn BiCheck
            { emit  = "EVEN(1.5)"
            , value = Right $ VNum 2
            , expr  = "even 1.5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "EVEN(3)"
            , value = Right $ VNum 4
            , expr  = "even 3;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "EVEN(2)"
            , value = Right $ VNum 2
            , expr  = "even 2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "EVEN(-1)"
            , value = Right $ VNum (-2)
            , expr  = "even (-1);"
            , defs  = L.empty
            }
      describe "EXP" $ do
        checkBuiltIn BiCheck
            { emit  = "EXP(1)"
            , value = Right $ VNum 2.718282
            , expr  = "exp 1;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "EXP(2)"
            , value = Right $ VNum 7.389056
            , expr  = "exp 2;"
            , defs  = L.empty
            }
      describe "FACT" $ do
        checkBuiltIn BiCheck
            { emit  = "FACT(5)"
            , value = Right $ VNum 120
            , expr  = "fact 5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "FACT(1.9)"
            , value = Right $ VNum 1
            , expr  = "fact 1.9;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "FACT(0)"
            , value = Right $ VNum 1
            , expr  = "fact 0;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "FACT(-1)"
            , value = Left $ EvBuiltInError "Factorial on negative"
            , expr  = "fact (-1);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "FACT(1)"
            , value = Right $ VNum 1
            , expr  = "fact 1;"
            , defs  = L.empty
            }
      describe "FLOOR" $ do
        checkBuiltIn BiCheck
            { emit  = "FLOOR([Score],1)"
            , value = Right $ VNum 2
            , expr  = "floor sc 1;"
            , defs  = "let sc = field \"Score\" :Num as 2.5;"
            }
        checkBuiltIn BiCheck
            { emit  = "FLOOR(SUM([Risk],[Criticality]),5)"
            , value = Right $ VNum 15
            , expr  = "floor (sum [risk,crit]) 5;"
            , defs  = "let risk = field \"Risk\" : Num as 8;\
                      \let crit = field \"Criticality\" : Num as 9.1;"
            }
      describe "INT" $ do
        checkBuiltIn BiCheck
            { emit  = "INT(8.9)"
            , value = Right $ VNum 8
            , expr  = "int 8.9;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "INT(-8.9)"
            , value = Right $ VNum (-9)
            , expr  = "int (-8.9);"
            , defs  = L.empty
            }
      describe "LN" $ do
        checkBuiltIn BiCheck
            { emit  = "LN(86)"
            , value = Right $ VNum 4.454347
            , expr  = "ln 86;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LN(EXP(1))"
            , value = Right $ VNum 1
            , expr  = "ln (exp 1);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LN(EXP(3))"
            , value = Right $ VNum 3
            , expr  = "ln (exp 3);"
            , defs  = L.empty
            }
      describe "LOG" $ do
        checkBuiltIn BiCheck
            { emit  = "LOG(10,10)"
            , value = Right $ VNum 1
            , expr  = "log 10 10;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LOG(8,2)"
            , value = Right $ VNum 3
            , expr  = "log 8 2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LOG(86,EXP(1))"
            , value = Right $ VNum 4.454347
            , expr  = "log 86 (exp 1);"
            , defs  = L.empty
            }
      describe "LOG10" $ do
        checkBuiltIn BiCheck
            { emit  = "LOG10(86)"
            , value = Right $ VNum 1.934498451
            , expr  = "log10 86;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LOG10(10)"
            , value = Right $ VNum 1
            , expr  = "log10 10;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LOG10(100000)"
            , value = Right $ VNum 5
            , expr  = "log10 100000;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "LOG10(10^5)"
            , value = Right $ VNum 5
            , expr  = "log10 10^5;"
            , defs  = L.empty
            }
      describe "MOD" $ do
        checkBuiltIn BiCheck
            { emit  = "MOD(3,2)"
            , value = Right $ VNum 1
            , expr  = "mod 3 2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "MOD(-3,2)"
            , value = Right $ VNum 1
            , expr  = "mod (-3) 2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "MOD(3,-2)"
            , value = Right $ VNum (-1)
            , expr  = "mod 3 (-2);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "MOD(-3,-2)"
            , value = Right $ VNum (-1)
            , expr  = "mod (-3) (-2);"
            , defs  = L.empty
            }
      describe "ODD" $ do
        checkBuiltIn BiCheck
            { emit  = "ODD(1.5)"
            , value = Right $ VNum 3
            , expr  = "odd 1.5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ODD(3)"
            , value = Right $ VNum 3
            , expr  = "odd 3;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ODD(2)"
            , value = Right $ VNum 3
            , expr  = "odd 2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ODD(-1)"
            , value = Right $ VNum (-1)
            , expr  = "odd (-1);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ODD(-2)"
            , value = Right $ VNum (-3)
            , expr  = "odd (-2);"
            , defs  = L.empty
            }
      describe "PI" $ do
        checkBuiltIn BiCheck
            { emit  = "PI()"
            , value = Right $ VNum 3.14159265358979
            , expr  = "pi;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "PI()/2"
            , value = Right $ VNum 1.570796327
            , expr  = "pi/2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "PI()*9"
            , value = Right $ VNum 28.27433388
            , expr  = "pi*(3^2)"
            , defs  = L.empty
            }
-- XXX: Need refs
--      describe "PRODUCT" $ do
      describe "QUOTIENT" $ do
        checkBuiltIn BiCheck
            { emit  = "QUOTIENT(42,5)"
            , value = Right $ VNum 8
            , expr  = "quot 42 5;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "QUOTIENT(11.5,2.15)"
            , value = Right $ VNum 5
            , expr  = "quot 11.5 2.15;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "QUOTIENT(-33,4.08)"
            , value = Right $ VNum (-8)
            , expr  = "quot (-33) 4.08;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "QUOTIENT([Rating],[Rank])"
            , value = Right $ VNum 15
            , expr  = "quot rt rk;"
            , defs  = "let rt = field \"Rating\" : Num as 92.68;\
                      \let rk = field \"Rank\" : Num as 6;"
            }
      describe "RADIANS" $
        checkBuiltIn BiCheck
            { emit  = "RADIANS(270)"
            , value = Right $ VNum 4.712389
            , expr  = "radians 270;"
            , defs  = L.empty
            }
      describe "ROUND" $ do
        checkBuiltIn BiCheck
            { emit  = "ROUND([Score],0)"
            , value = Right $ VNum 23
            , expr  = "round sc 0;"
            , defs  = "let sc = field \"Score\" : Num as 23.357;"
            }
        checkBuiltIn BiCheck
            { emit  = "ROUND(SUM([Risk],[Criticality]),2)"
            , value = Right $ VNum 17.08
            , expr  = "round (sum [risk,crit]) 2;"
            , defs  = "let risk = field \"Risk\" : Num as 12.725;\
                      \let crit = field \"Criticality\" : Num as 4.351;"
            }
      describe "ROUNDDOWN" $ do
        checkBuiltIn BiCheck
            { emit  = "ROUNDDOWN(3.2,0)"
            , value = Right $ VNum 3
            , expr  = "rounddown 3.2 0;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDDOWN(76.9,0)"
            , value = Right $ VNum 76
            , expr  = "rounddown 76.9 0;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDDOWN(3.14159,3)"
            , value = Right $ VNum 3.141
            , expr  = "rounddown 3.14159 3;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDDOWN(-3.14159,1)"
            , value = Right $ VNum (-3.1)
            , expr  = "rounddown (-3.14159) 1;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDDOWN(31415.92654,-2)"
            , value = Right $ VNum 31400
            , expr  = "rounddown 31415.92654 (-2);"
            , defs  = L.empty
            }
      describe "ROUNDUP" $ do
        checkBuiltIn BiCheck
            { emit  = "ROUNDUP(3.2,0)"
            , value = Right $ VNum 4
            , expr  = "roundup 3.2 0;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDUP(76.9,0)"
            , value = Right $ VNum 77
            , expr  = "roundup 76.9 0;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDUP(3.14159,3)"
            , value = Right $ VNum 3.142
            , expr  = "roundup 3.14159 3;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDUP(-3.14159,1)"
            , value = Right $ VNum (-3.2)
            , expr  = "roundup (-3.14159) 1;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "ROUNDUP(31415.92654,-2)"
            , value = Right $ VNum 31500
            , expr  = "roundup 31415.92654 (-2);"
            , defs  = L.empty
            }
      describe "SIGN" $ do
        checkBuiltIn BiCheck
            { emit  = "SIGN(10)"
            , value = Right $ VNum 1
            , expr  = "sign 10;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "SIGN(4-4)"
            , value = Right $ VNum 0
            , expr  = "sign (4-4);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "SIGN(-0.00001)"
            , value = Right $ VNum (-1)
            , expr  = "sign (-0.00001);"
            , defs  = L.empty
            }
      describe "SIN" $ do
        checkBuiltIn BiCheck
            { emit  = "SIN(PI())"
            , value = Right $ VNum 0
            , expr  = "sin pi;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "SIN(PI()/2)"
            , value = Right $ VNum 1
            , expr  = "sin pi/2;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "SIN(30*PI()/180)"
            , value = Right $ VNum 0.5
            , expr  = "sin 30*pi/180;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "SIN(RADIANS(30))"
            , value = Right $ VNum 0.5
            , expr  = "sin (radians 30);"
            , defs  = L.empty
            }
      describe "SINH" $ do
        checkBuiltIn BiCheck
            { emit  = "SINH(1)"
            , value = Right $ VNum 1.175201194
            , expr  = "sinh 1;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "SINH(-1)"
            , value = Right $ VNum (-1.175201194)
            , expr  = "sinh (-1);"
            , defs  = L.empty
            }
      describe "SQRT" $
        checkBuiltIn BiCheck
            { emit  = "SQRT(16)"
            , value = Right $ VNum 4
            , expr  = "sqrt 16;"
            , defs  = L.empty
            }
      describe "SUM" $ do
        checkBuiltIn BiCheck
            { emit  = "SUM(3,[Risk])"
            , value = Right $ VNum 15
            , expr  = "sum [3,risk];"
            , defs  = "let risk = field \"Risk\" : Num as 12;"
            }
        checkBuiltIn BiCheck
            { emit  = "SUM([Risk],[Criticality])"
            , value = Right $ VNum 19
            , expr  = "sum [risk,crit];"
            , defs  = "let risk = field \"Risk\" : Num as 12;\
                      \let crit = field \"Criticality\" : Num as 7;"
            }
        {-- REF not implemented
        checkBuiltIn BiCheck
            { emit  = "SUM(REF([Orders],[Price]))"
            , value = Right $ VNum 202.94
            , expr  = undefined
            , defs  = undefined
            }
        -- SELECTEDVALUENUMBER not implemented
        checkBuiltIn BiCheck
            { emit  = "SUM(SELECTEDVALUENUMBER([Key Factors]))"
            , value = Right $ VNum 25
            , expr  = undefined
            , defs  = undefined
            }
        -}
      describe "SUMSQ" $
        checkBuiltIn BiCheck
            { emit  = "SUMSQ(3,4)"
            , value = Right $ VNum 25
            , expr  = "sumsq 3 4;"
            , defs  = L.empty
            }
      describe "TAN" $ do
        checkBuiltIn BiCheck
            { emit  = "TAN(0.784)"
            , value = Right $ VNum 0.99920
            , expr  = "tan 0.784;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "TAN(45*PI()/180)"
            , value = Right $ VNum 1
            , expr  = "tan (45*pi/180)"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "TAN(RADIANS(45))"
            , value = Right $ VNum 1
            , expr  = "tan (radians 45);"
            , defs  = L.empty
            }
      describe "TANH" $ do
        checkBuiltIn BiCheck
            { emit  = "TANH(-2)"
            , value = Right $ VNum (-0.96403)
            , expr  = "tanh (-2);"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "TANH(0)"
            , value = Right $ VNum 0
            , expr  = "tanh 0;"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "TANH(0.5)"
            , value = Right $ VNum 0.462117
            , expr  = "tanh 0.5;"
            , defs  = L.empty
            }
      describe "TRUNC" $ do
        checkBuiltIn BiCheck
            { emit  = "TRUNC([Score],0)"
            , value = Right $ VNum 3
            , expr  = "trunc sc 0;"
            , defs  = "let sc = field \"Score\" : Num as 3.427;"
            }
        checkBuiltIn BiCheck
            { emit  = "TRUNC([Score],1)"
            , value = Right $ VNum 3.4
            , expr  = "trunc sc 1;"
            , defs  = "let sc = field \"Score\" : Num as 3.427;"
            }
        {-- TODAY not implemented
        checkBuiltIn BiCheck
            { emit  = "IF(TRUNC([Ship Date-Time]) = TODAY( ), “Shipped Today”, “Not Shipped Today”)"
            , value = Right $ VText "Shipped Today"
            , expr  = "if (trunc sdt == today then \
                      \    \"Shipped Today\"\
                      \else\
                      \    \"Not shipped Today\";"
            , defs  = L.empty
            }
        -}
