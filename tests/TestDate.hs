{-# LANGUAGE OverloadedStrings #-}
module TestDate where

import           Test.Hspec
import qualified Data.Text.Lazy as L
import           Data.Time (parseTime)
import           System.Locale (defaultTimeLocale)

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
                 \ArDate \"08/10/2010 07:21:00\";\n"
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
            , value = Right (VNum 36)
            , expr  = "datedif (ArDate \"10/21/2010\") fp Day;"
            , defs  = "let fp = field \"First Published\" : Date as \
                      \ArDate \"11/26/2010\";"
            }
-}
        checkBuiltIn BiCheck
            { emit  = "DATEDIF([First Published],[Last Updated],DAY)"
            , value = Right (VNum 0)
            , expr  = "datedif fp lu Day"
            , defs  = "let fp = field \"First Published\" : Date as \
                     \ArDate \"11/26/2010 23:59:00\";\
                     \let lu = field \"Last Updated\" : Date as \
                     \ArDate \"11/27/2010 00:01:00\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEDIF([First Published],[Last Updated],HOUR)"
            , value = Right (VNum 50)
            , expr  = "datedif fp lu Hour"
            , defs  = "let fp = field \"First Published\" : Date as \
                      \ArDate \"10/01/2010 08:05:00\";\
                      \let lu = field \"Last Updated\" : Date as \
                      \ArDate \"10/03/2010 10:32:00\";"
            }
        checkBuiltIn BiCheck
            { emit  = "DATEDIF([First Published],[Last Updated],MINUTE)"
            , value = Right (VNum 147)
            , expr  = "datedif fp lu Minute"
            , defs  = "let fp = field \"First Published\" : Date as \
                       \ArDate \"10/01/2010 08:05:00\";\
                       \let lu = field \"Last Updated\" : Date as \
                       \ArDate \"10/01/2010 10:32:00\";"
            }
--      describe "DATEFORMAT"·
      describe "DATETIMEVALUE" $ do
        -- XXX: Archer Docs say 40453, but actual is 40451!!!
        checkBuiltIn BiCheck
            { emit  = "DATETIMEVALUE(\"10/02/2010\")"
            , value = Right (VNum 40451)
            , expr  = "datetimevalue (ArDate \"10/02/2010\");"
            , defs  = L.empty
            }
        checkBuiltIn BiCheck
            { emit  = "DATETIMEVALUE(\"10/02/2010 01:50:00\")"
            , value = Right (VNum 40451.08)
            , expr  = "datetimevalue (ArDate \"10/02/2010 01:50:00\");"
            , defs  = L.empty
            }
      describe "DAY" $
        checkBuiltIn BiCheck
            { emit  = "DAY([Logged])"
            , value = Right (VNum 13)
            , expr  = "day lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                     \ArDate \"07/13/2010 10:45:00\";"
            }
      describe "HOUR" $
        checkBuiltIn BiCheck
            { emit  = "HOUR([Logged])"
            , value = Right (VNum 14)
            , expr  = "hour lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                     \ArDate \"07/13/2006 14:45:00\";"
            }
      describe "MINUTE" $ do
        checkBuiltIn BiCheck
            { emit  = "MINUTE([Logged])"
            , value = Right (VNum 45)
            , expr  = "minute lg;"
            , defs  = "let lg = field \"Logged\" : Date as \
                      \ArDate \"07/13/2006 14:45:00\";"
            }
{- XXX: We don't handle empty fields correctly yet.
        checkBuiltIn BiCheck
            { emit  = "MINUTE([Patch Date])"
            , value = Right (VNum 0)
            , expr  = "minute lg;"
            , defs  = "let pd = field \"Patch Date\" : Date;"
            }
-}
      describe "MONTHNAME" $
        checkBuiltIn BiCheck
            { emit  = "MONTHNAME([Due Date])"
            , value = Right (VText "July")
            , expr  = "monthname dd;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"07/13/2010 14:45:00\";"
            }
--      describe "now"
      describe "QUARTER" $
        checkBuiltIn BiCheck
            { emit  = "QUARTER([Due Date])"
            , value = Right $ VNum 4
            , expr  = "quarter dd;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"12/15/2010 20:00:00\";"
            }
      describe "WEEKDAY" $
        checkBuiltIn BiCheck
            { emit  = "WEEKDAY([Due Date])"
            , value = Right $ VText "Wednesday"
            , expr  = "weekday dd;"
            , defs  = "let dd = field \"Due Date\" : Date as \
                      \ArDate \"12/15/2010 20:00:00\";"
            }

