{-# LANGUAGE OverloadedStrings #-}
module TestFinance where

import           Data.Monoid ((<>))
import           Test.Hspec

import           Pretty()
import           Types

import           TestCommon

testBuiltInFinance :: Spec
testBuiltInFinance =
    describe "Built-in Finance Functions" $ do
      describe "DB" $ do
        let dbdef = "let ic = field \"Initial Cost\" : Num as 1000000;\
                    \let sv = field \"Salvage Value\" : Num as 100000;\
                    \let lf = field \"Lifetime in Years\" : Num as 6;\
                    \let mn = field \"Month\" : Num as 7;"
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 186083.33
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 1;"
            }
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 259639.42
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 2;"
            }
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 176814.44
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 3;"
            }
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 120410.64
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 4;"
            }
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 81999.64
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 5;"
            }
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 55841.76
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 6;"
            }
        checkBuiltIn BiCheck
            { emit  = "DB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Month])"
            , value = Right $ VNum 15845.10
            , expr  = "db ic sv lf pd mn;"
            , defs  = dbdef <> "let pd = field \"Period in Years\" : Num as 7;"
            }
      -- XXX: The DDB section of the docs is pretty bunk. The first example
      -- is bogus, and the rest refer to the function as DB.
      describe "DDB" $ do
        {- Bunk example}
        checkBuiltIn BiCheck
            { emit  = "DDB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years])"
            , value = Right $ VNum 1.32
            , expr  = undefined
            , defs  = undefined
            }
        -}
        checkBuiltIn BiCheck
            { emit  = "DDB([Initial Cost],[Salvage Value],[Lifetime in Months],[Period in Months],[Factor])"
            , value = Right $ VNum 40
            , expr  = "ddb ic sv lt per fac;"
            , defs  = "let ic = field \"Initial Cost\" : Num as 2400;\
                      \let sv = field \"Salvage Value\" : Num as 300;\
                      \let lt = field \"Lifetime in Months\" : Num as 120;\
                      \let per = field \"Period in Months\" : Num as 1;\
                      \let fac = field \"Factor\" : Num as 2;"
            }
        checkBuiltIn BiCheck
            { emit  = "DDB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Factor])"
            , value = Right $ VNum 480
            , expr  = "ddb ic sv lt per fac;"
            , defs  = "let ic = field \"Initial Cost\" : Num as 2400;\
                      \let sv = field \"Salvage Value\" : Num as 300;\
                      \let lt = field \"Lifetime in Years\" : Num as 10;\
                      \let per = field \"Period in Years\" : Num as 1;\
                      \let fac = field \"Factor\" : Num as 2;"
            }
        checkBuiltIn BiCheck
            { emit  = "DDB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Factor])"
            , value = Right $ VNum 306
            , expr  = "ddb ic sv lt per fac;"
            , defs  = "let ic = field \"Initial Cost\" : Num as 2400;\
                      \let sv = field \"Salvage Value\" : Num as 300;\
                      \let lt = field \"Lifetime in Years\" : Num as 10;\
                      \let per = field \"Period in Years\" : Num as 2;\
                      \let fac = field \"Factor\" : Num as 1.5;"
            }
        checkBuiltIn BiCheck
            { emit  = "DDB([Initial Cost],[Salvage Value],[Lifetime in Years],[Period in Years],[Factor])"
            , value = Right $ VNum 22.12
            , expr  = "ddb ic sv lt per fac;"
            , defs  = "let ic = field \"Initial Cost\" : Num as 2400;\
                      \let sv = field \"Salvage Value\" : Num as 300;\
                      \let lt = field \"Lifetime in Years\" : Num as 10;\
                      \let per = field \"Period in Years\" : Num as 10;\
                      \let fac = field \"Factor\" : Num as 2;"
            }
      describe "NPV" $ do
        checkBuiltIn BiCheck
            { emit  = "NPV([Rate],[Values])"
            , value = Right $ VNum 1188.44
            , expr  = "npv rt vs;"
            , defs  = "let rt = field \"Rate\" : Num as 0.10;\
                      \let vs = field \"Values\" : List Num as [(-10000),3000,4200,6800];"
            }
        checkBuiltIn BiCheck
            { emit  = "NPV([Rate],[Values])-40000"
            , value = Right $ VNum 1922.06
            , expr  = "(npv rt vs) - 40000;"
            , defs  = "let rt = field \"Rate\" : Num as 0.08;\
                      \let vs = field \"Values\" : List Num as [8000,9200,10000,12000,14500];"
            }
        checkBuiltIn BiCheck
            { emit  = "NPV([Rate],[Values])-40000"
            , value = Right $ VNum (-3749.47)
            , expr  = "(npv rt vs) - 40000;"
            , defs  = "let rt = field \"Rate\" : Num as 0.08;\
                      \let vs = field \"Values\" : List Num as [8000,9200,10000,12000,14500,-9000];"
            }
      describe "SLN" $
        checkBuiltIn BiCheck
            { emit  = "SLN([Cost],[Salvage Value],[Years of Useful Life])"
            , value = Right $ VNum 2250
            , expr  = "sln cs sv ul;"
            , defs  = "let cs = field \"Cost\" : Num as 30000;\
                      \let sv = field \"Salvage Value\" : Num as 7500;\
                      \let ul = field \"Years of Useful Life\" : Num as 10;"
            }
