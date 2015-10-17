{-# LANGUAGE OverloadedStrings #-}
module TestDate where

import           Test.Hspec
import qualified Data.Text.Lazy as L
import           Data.Time (parseTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)

import           TestCommon

import           Types

-- | Date string into a Value
toDate :: String -> Either EvalError Value
toDate x = case parseTime defaultTimeLocale "%FT%T%Z" x of
    Just d -> Right $ VDate d
    Nothing -> Left $ EvVarNotFound "error"

testBuiltInDate :: Spec
testBuiltInDate =
    describe "Built-in Date Functions" $ do
      describe "DATEADD" $ do
        let fd = "let fp = field \"First Published\" : Date as \
                 \ArDate \"8/10/2010 7:21 AM\";\n"
        checkBuiltIn BiCheck
            { emit  = "DATEADD(DAY,10,[First Published])"
            , value = toDate "2010-08-20T07:21:00Z"
            , expr  = "dateadd Day 10 fp;"
            , defs  = fd
            }
        checkBuiltIn BiCheck
            { emit  = "DATEADD(HOUR,6,[First Published])"
            , value = toDate "2010-08-10T13:21:00Z"
            , expr  = "dateadd Hour 6 fp;"
            , defs  = fd
            }
        checkBuiltIn BiCheck
            { emit  = "DATEADD(MINUTE,30,[First Published])"
            , value = toDate "2010-08-10T07:51:00Z"
            , expr  = "dateadd Minute 30 fp;"
            , defs  = fd
            }
      describe "DATEDIF" $ do
{- XXX: Constant needs to be wrapped with DATETIMEVALUE, which we don't do yet.
        checkBuiltIn BiCheck
            { emit  = "DATEDIF(DATETIMEVALUE(\"10/21/2010\"),[First Published])"
            , value = Right $ VNum 36
            , expr  = "datedif (ArDate \"10/21/2010\") fp Day;"
            , defs  = "let fp = field \"First Published\" : Date as \
                      \ArDate \"11/26/2010\";"
            }
-}
        checkBuiltIn BiCheck
            { emit  = "DATEDIF([First Published],[Last Updated],DAY)"
            , value = Right $ VNum 0
            , expr  = "datedif fp lu Day"
            , defs  = "let fp = field \"First Published\" : Date as \
                     \ArDate \"11/26/2010 11:59 PM\";\
                     \let lu = field \"Last Updated\" : Date as \
                     \ArDate \"11/27/2010 12:01 AM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEDIF([First Published],[Last Updated],HOUR)"
            , value = Right $ VNum 50
            , expr  = "datedif fp lu Hour"
            , defs  = "let fp = field \"First Published\" : Date as \
                      \ArDate \"10/1/2010 8:05 AM\";\
                      \let lu = field \"Last Updated\" : Date as \
                      \ArDate \"10/3/2010 10:32 AM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEDIF([First Published],[Last Updated],MINUTE)"
            , value = Right $ VNum 147
            , expr  = "datedif fp lu Minute"
            , defs  = "let fp = field \"First Published\" : Date as \
                       \ArDate \"10/1/2010 8:05 AM\";\
                       \let lu = field \"Last Updated\" : Date as \
                       \ArDate \"10/1/2010 10:32 AM\";"
            }
      describe "DATEFORMAT" $ do
        checkBuiltIn BiCheck
            { emit  = "DATEFORMAT([First Published],\"M/d/yyyy h:mm tt\")"
            , value = Right $ VText "8/20/2010 7:21 AM"
            , expr  = "dateformat fp \"M/d/yyyy h:mm tt\";"
            , defs  = "let fp = field \"First Published\" : Date as \
                       \ArDate \"8/20/2010 7:21 AM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEFORMAT([Last Updated],\"M/d/yyyy HH:mm\")"
            , value = Right $ VText "12/19/2010 14:51"
            , expr  = "dateformat lu \"M/d/yyyy HH:mm\";"
            , defs  = "let lu = field \"Last Updated\" : Date as \
                      \ArDate \"12/19/2010 2:51 PM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEFORMAT([Fake Now],\"h:mm tt\")"
            , value = Right $ VText "5:12 AM"
            , expr  = "dateformat fn \"h:mm tt\";"
            , defs  = "let fn = field \"Fake Now\" : Date as \
                      \ArDate \"8/6/2010 5:12 AM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEFORMAT([Start],\"hh:mm t\")"
            , value = Right $ VText "06:48 P"
            , expr  = "dateformat st \"hh:mm t\";"
            , defs  = "let st = field \"Start\" : Date as \
                      \ArDate \"9/19/2010 6:48 PM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEFORMAT([Stop],\"H\")"
            , value = Right $ VText "19"
            , expr  = "dateformat st \"H\";"
            , defs  = "let st = field \"Stop\" : Date as \
                      \ArDate \"4/8/2010 7:00 PM\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEFORMAT([Logged],\"m\")"
            , value = Right $ VText "57"
            , expr  = "dateformat lg \"m\";"
            , defs  = "let lg = field \"Logged\" : Date as \
                      \ArDate \"12/29/2010 3:57 PM\";"
            }
      describe "DATETIMEVALUE" $ do
        -- XXX: Archer Docs say 40453, but actual is 40451!!!
        checkBuiltIn BiCheck
            { emit  = "DATETIMEVALUE(\"10/02/2010\")"
            , value = Right $ VNum 40451
            , expr  = "datetimevalue (ArDate \"10/02/2010\");"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "DATETIMEVALUE(\"10/02/2010 01:50:00\")"
            , value = Right $ VNum 40451.08
            , expr  = "datetimevalue (ArDate \"10/02/2010 01:50\");"
            , defs  = L.empty
            }
      describe "DAY" $
        checkBuiltIn BiCheck
            { emit  = "DAY([Logged])"
            , value = Right $ VNum 13
            , expr  = "day lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                     \ArDate \"7/13/2010 10:45 AM\";"
            }
      describe "HOUR" $
        checkBuiltIn BiCheck
            { emit  = "HOUR([Logged])"
            , value = Right $ VNum 14
            , expr  = "hour lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                     \ArDate \"7/13/2006 2:45 PM\";"
            }
      describe "MINUTE" $ do
        checkBuiltIn BiCheck
            { emit  = "MINUTE([Logged])"
            , value = Right $ VNum 45
            , expr  = "minute lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                      \ArDate \"7/13/2006 2:45 PM\";"
            }
{- XXX: We don't handle empty fields correctly yet.
        checkBuiltIn BiCheck
            { emit  = "MINUTE([Patch Date])"
            , value = Right $ VNum 0
            , expr  = "minute lg;"
            , defs  = "let pd = field \"Patch Date\" : Date;"
            }
-}
      describe "MONTH" $
        checkBuiltIn BiCheck
            { emit  = "MONTH([Logged])"
            , value = Right $ VNum 7
            , expr  = "month lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                      \ArDate \"7/13/2010 2:45 PM\";"
            }
      describe "MONTHNAME" $
        checkBuiltIn BiCheck
            { emit  = "MONTHNAME([Due Date])"
            , value = Right $ VText "July"
            , expr  = "monthname dd;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"7/13/2010 2:45 PM\";"
            }
--      describe "now"
      describe "QUARTER" $
        checkBuiltIn BiCheck
            { emit  = "QUARTER([Due Date])"
            , value = Right $ VNum 4
            , expr  = "quarter dd;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"12/15/2010 8:00 PM\";"
            }
      describe "WEEKDAY" $
        checkBuiltIn BiCheck
            { emit  = "WEEKDAY([Due Date])"
            , value = Right $ VText "Wednesday"
            , expr  = "weekday dd;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"12/15/2010 8:00 pm\";"
            }
      describe "WEEKNUMBER" $ do
        checkBuiltIn BiCheck
            { emit  = "WEEKNUMBER([Due Date],SUNDAY)"
            , value = Right $ VNum 38
            , expr  = "weeknumber dd Sunday;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"9/14/2008\";"
            }
        checkBuiltIn BiCheck
            { emit  = "WEEKNUMBER([Due Date],MONDAY)"
            , value = Right $ VNum 37
            , expr  = "weeknumber dd Monday;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"9/14/2008\";"
            }
      describe "YEAR" $
        checkBuiltIn BiCheck
            { emit  = "YEAR([First Published])"
            , value = Right $ VNum 2010
            , expr  = "year fp;"
            , defs  = "let fp = field \"First Published\" : Date as \
                      \ArDate \"11/26/2010\";"
            }
