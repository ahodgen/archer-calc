{-# LANGUAGE OverloadedStrings #-}
module TestStats where

import qualified Data.Text.Lazy as L
import           Test.Hspec

import           Pretty()
import           Types

import           TestCommon

testBuiltInStats :: Spec
testBuiltInStats =
    describe "Built-in Statistics Functions" $ do
      describe "BINOMDIST" $
        checkBuiltIn BiCheck
            { emit  = "BINOMDIST([Successes],[Trials],[Probability],FALSE)"
            , value = Right $ VNum 0.205078
            , expr  = "binomdist suc trls prob False;"
            , defs  = "let suc = field \"Successes\" : Num as 6;\
                      \let trls = field \"Trials\" : Num as 10;\
                      \let prob = field \"Probability\" : Num as 0.5;"
            }
      describe "CHIDIST" $
        checkBuiltIn BiCheck
            { emit  = "CHIDIST(18.307,10)"
            , value = Right $ VNum 0.050001
            , expr  = "chidist 18.307 10;"
            , defs  = L.empty
            }
{-
      describe "CHIINV" $
        checkBuiltIn BiCheck
            { emit  = "CHIINV(0.50001,10)"
            , value = Right $ VNum 18.3069735
            , expr  = "chiinv 0.50001 10;"
            , defs  = L.empty
            }
-}
      describe "CONFIDENCE" $ do
        checkBuiltIn BiCheck
            { emit  = "CONFIDENCE([Significance],[Standard Deviation],[Sample Size])"
            , value = Right $ VNum 0.692952
            , expr  = "confidence sig std ss;"
            , defs  = "let sig = field \"Significance\" : Num as 0.05;\
                      \let std = field \"Standard Deviation\" : Num as 2.5;\
                      \let ss  = field \"Sample Size\" : Num as 50;"
            }
        -- XXX: This isn't from examples
        checkBuiltIn BiCheck
            { emit  = "CONFIDENCE(0.05,30,1000)"
            , value = Right $ VNum 1.86
            , expr  = "confidence 0.05 30 1000;"
            , defs  = L.empty
            }
      describe "EXPONDIST" $ do
        checkBuiltIn BiCheck
            { emit  = "EXPONDIST([Function Value],[Parameter Value],TRUE)"
            , value = Right $ VNum 0.864665
            , expr  = "expondist fv pv True;"
            , defs  = "let fv = field \"Function Value\" : Num as 0.2;\
                      \let pv = field \"Parameter Value\" : Num as 10;"
            }
        checkBuiltIn BiCheck
            { emit  = "EXPONDIST([Function Value],[Parameter Value],FALSE)"
            , value = Right $ VNum 1.353353
            , expr  = "expondist fv pv False;"
            , defs  = "let fv = field \"Function Value\" : Num as 0.2;\
                      \let pv = field \"Parameter Value\" : Num as 10;"
            }
      describe "FDIST" $
        checkBuiltIn BiCheck
            { emit  = "FDIST(15.20686486,[Numerator Degrees of Freedom],[Denominator Degrees of Freedom])"
            , value = Right $ VNum 0.01
            , expr  = "fdist 15.20686486 df1 df2;"
            , defs  = "let df1 = field \"Numerator Degrees of Freedom\" : Num as 6;\
                      \let df2 = field \"Denominator Degrees of Freedom\" : Num as 4;"
            }
      describe "FISHER" $
        checkBuiltIn BiCheck
            { emit  = "FISHER(0.75)"
            , value = Right $ VNum 0.972955
            , expr  = "fisher 0.75;"
            , defs  = L.empty
            }
      describe "FISHERINV" $
        checkBuiltIn BiCheck
            { emit  = "FISHERINV(0.972955)"
            , value = Right $ VNum 0.75
            , expr  = "fisherinv 0.972955;"
            , defs  = L.empty
            }
      describe "GAMMADIST" $ do
        checkBuiltIn BiCheck
            { emit  = "GAMMADIST([Value to Evaluate Distribution],[Alpha],[Beta],FALSE)"
            , value = Right $ VNum 0.03263913
            , expr  = "gammadist ved a b False;"
            , defs  = "let ved = field \"Value to Evaluate Distribution\" : Num as 10.00001131;\
                      \let a = field \"Alpha\" : Num as 9;\
                      \let b = field \"Beta\" : Num as 2;"
            }
        checkBuiltIn BiCheck
            { emit  = "GAMMADIST([Value to Evaluate Distribution],[Alpha],[Beta],TRUE)"
            , value = Right $ VNum 0.068094
            , expr  = "gammadist ved a b True;"
            , defs  = "let ved = field \"Value to Evaluate Distribution\" : Num as 10.00001131;\
                      \let a = field \"Alpha\" : Num as 9;\
                      \let b = field \"Beta\" : Num as 2;"
            }
      describe "GAMMAINV" $
        checkBuiltIn BiCheck
            { emit  = "GAMMAINV([Probability],[Alpha],[Beta])"
            , value = Right $ VNum 10.00001131
            , expr  = "gammainv p a b;"
            , defs  = "let p = field \"Probability\" : Num as 0.068094;\
                      \let a = field \"Alpha\" : Num as 9;\
                      \let b = field \"Beta\" : Num as 2;"
            }
      describe "GAMMALN" $
        checkBuiltIn BiCheck
            { emit  = "GAMMALN(4)"
            , value = Right $ VNum 1.791759
            , expr  = "gammaln 4;"
            , defs  = L.empty
            }
      describe "HYPGEOMDIST" $
        checkBuiltIn BiCheck
            { emit  = "HYPGEOMDIST([Number of Caramels in Sample],[Sample Size],[Total Number of Caramels],[Total Chocolates])"
            , value = Right $ VNum 0.363261
            , expr  = "hypgeomdist nc ss tc tch;"
            , defs  = "let nc = field \"Number of Caramels in Sample\" : Num as 1;\
                      \let ss = field \"Sample Size\" : Num as 4;\
                      \let tc = field \"Total Number of Caramels\" : Num as 8;\
                      \let tch = field \"Total Chocolates\" : Num as 20;"
            }
