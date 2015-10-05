{-# LANGUAGE OverloadedStrings #-}
module TestText where

import           Test.Hspec

import           Pretty()
import           Types

import           TestCommon

testBuiltInText :: Spec
testBuiltInText =
    describe "Built-in Text Functions" $ do
      describe "FIND" $ do
        checkBuiltIn BiCheck
            { emit  = "FIND(\"Sci\",[Subject],0)"
            , value = Right $ VNum 10
            , expr  = "find \"Sci\" sub 0;"
            , defs  = "field sub = \"Subject\" : Text as \"Arts and Sciences\";"
            }
        checkBuiltIn BiCheck
            { emit  = "FIND(\"s\",[Subject],5)"
            , value = Right $ VNum 17
            , expr  = "find \"s\" sub 5;"
            , defs  = "field sub = \"Subject\" : Text as \"Arts and Sciences\";"
            }
      describe "LEFT" $ do
        checkBuiltIn BiCheck
            { emit  = "LEFT([Text],4)"
            , value = Right $ VText "Sale"
            , expr  = "left txt 4;"
            , defs  = "field txt = \"Text\" : Text as \"Sale Price\";"
            }
        checkBuiltIn BiCheck
            { emit  = "LEFT([Text],1)"
            , value = Right $ VText "S"
            , expr  = "left txt 1;"
            , defs  = "field txt = \"Text\" : Text as \"Sweden\";"
            }
      describe "LEN" $
        checkBuiltIn BiCheck
            { emit  = "LEN([Last Name])"
            , value = Right $ VNum 5
            , expr  = "len ln;"
            , defs  = "field ln = \"Last Name\" : Text as \"Jones\";"
            }
      describe "LOWER" $ do
        checkBuiltIn BiCheck
            { emit  = "LOWER([Name])"
            , value = Right $ VText "jake miller"
            , expr  = "lower nm;"
            , defs  = "field nm = \"Name\" : Text as \"Jake Miller\";"
            }
        checkBuiltIn BiCheck
            { emit  = "LOWER([Email Address])"
            , value = Right $ VText "suzy.williams@shore2shore.org"
            , expr  = "lower ea;"
            , defs  = "field ea = \"Email Address\" : Text \
                      \as \"SUZY.WILLIAMS@Shore2Shore.org\";"
            }
      describe "PROPER" $ do
        checkBuiltIn BiCheck
            { emit  = "PROPER([Last Name])"
            , value = Right $ VText "Jane Pearson-Wyatt"
            , expr  = "proper ln;"
            , defs  = "field ln = \"Last Name\" : Text \
                      \as \"jane pearson-wyatt\";"
            }
        checkBuiltIn BiCheck
            { emit  = "PROPER([Last Name])"
            , value = Right $ VText "O’Neil"
            , expr  = "proper ln;"
            , defs  = "field ln = \"Last Name\" : Text as \"O’NEIL\";"
            }
        checkBuiltIn BiCheck
            { emit  = "PROPER([Last Name])"
            , value = Right $ VText "St. John"
            , expr  = "proper ln;"
            , defs  = "field ln = \"Last Name\" : Text as \"ST. JOHN\";"
            }
        checkBuiltIn BiCheck
            { emit  = "PROPER([Web Page])"
            , value = Right $ VText "Www.Archer-Tech.Com"
            , expr  = "proper wp;"
            , defs  = "field wp = \"Web Page\" : Text as \"www.archer-tech.com\";"
            }
        checkBuiltIn BiCheck
            { emit  = "PROPER([Equipment Note])"
            , value = Right $ VText "This Is Mike’S Laptop."
            , expr  = "proper en;"
            , defs  = "field en = \"Equipment Note\" : Text as\
                      \ \"This is Mike’s laptop.\";"
            }
      describe "RIGHT" $
        checkBuiltIn BiCheck
            { emit  = "RIGHT([Department Name],4)"
            , value = Right $ VText "ting"
            , expr  = "right dn 4;"
            , defs  = "field dn = \"Department Name\" : Text as \"Marketing\";"
            }
{-        checkBuiltIn BiCheck
            { emit  = "RIGHT([Department Name],(-1))"
            , value = Left $ EvBuiltInError ""
            , expr  = "right dn -1;"
            , defs  = "field dn = \"Department Name\" : Text as \"Marketing\";"
            } -}
      describe "SUBSTRING" $
        checkBuiltIn BiCheck
            { emit  = "SUBSTRING([Department Name],1,4)"
            , value = Right $ VText "Mark"
            , expr  = "substring dn 1 4;"
            , defs  = "field dn = \"Department Name\" : Text as \"Marketing\";"
            }
      describe "TRIM" $
        checkBuiltIn BiCheck
            { emit  = "TRIM([Asset Description])"
            , value = Right $ VText "The HR-DB Server is used to store our human resources information."
            , expr  = "trim ad;"
            , defs  = "field ad = \"Asset Description\" : Text as \" The HR-DB Server is used to store our human resources information. \";"
            }
      describe "UPPER" $ do
        checkBuiltIn BiCheck
            { emit  = "UPPER([Name])"
            , value = Right $ VText "JAKE MILLER"
            , expr  = "upper nm;"
            , defs  = "field nm = \"Name\" : Text as \"Jake Miller\";"
            }
        checkBuiltIn BiCheck
            { emit  = "UPPER([Web Site])"
            , value = Right $ VText "WWW.ARCHER-TECH.COM"
            , expr  = "upper ws;"
            , defs  = "field ws = \"Web Site\" : Text as \"www.archer-tech.com\";"
            }
