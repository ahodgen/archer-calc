module BuiltIn.Date
    ( builtInDate
    , helpDate
    ) where

-- import           Control.Monad.Except
import qualified Data.Map as M
import           Data.Time
import           System.IO.Unsafe
import           Data.Time.Locale.Compat (defaultTimeLocale)

import           Syntax
import           Type
import           Types

-- TODO:
-- DATEFORMAT
-- TODAY
-- WEEKNUMBER (Need type WeekStart (Sunday and Monday))

infError :: Interpreter EvalError Value
infError = error $ "Received unexpected types. Either the built-in was " ++
                   "improperly defined or type inference failed."

dateAdd :: [Value] -> Interpreter EvalError Value
dateAdd [VTimUn unit, VNum intv, VDate dt] =
    case unit of
        Day  -> return $ VDate . secAdd $ realToFrac (86400*intv)
        Hour -> return . VDate . secAdd $ realToFrac (3600*intv)
        Min  -> return . VDate . secAdd $ realToFrac (60*intv)
  where
    secAdd x = addUTCTime x dt
dateAdd _ = infError

dateDif :: [Value] -> Interpreter EvalError Value
dateDif [VDate st, VDate end, VTimUn unit] = case unit of
    Day  -> return $ VNum days
    Hour -> return $ VNum hours
    Min  -> return $ VNum mins
  where
    days  = fromIntegral $ secs `div` 86400
    hours = fromIntegral $ secs `div` 3600
    mins  = fromIntegral $ secs `div` 60
    secs :: Integer
    secs = floor $ toRational $ diffUTCTime end st
dateDif _  = infError

dateTimeVal :: [Value] -> Interpreter EvalError Value
dateTimeVal [VDate dt] = return $ VNum $ realToFrac (diffUTCTime dt start) / 86400
  where
    start = UTCTime (fromGregorian 1900 01 01) 0
dateTimeVal _  = infError

dayOf :: [Value] -> Interpreter EvalError Value
dayOf [VDate dt] = return . VNum $ fromIntegral d
  where
    (_,_,d) = toGregorian $ utctDay dt
dayOf _ = infError

monthOf :: [Value] -> Interpreter EvalError Value
monthOf [VDate dt] = return . VNum $ fromIntegral m
  where
    (_,m,_) = toGregorian $ utctDay dt
monthOf _ = infError

yearOf :: [Value] -> Interpreter EvalError Value
yearOf [VDate dt] = return . VNum $ fromIntegral y
  where
    (y,_,_) = toGregorian $ utctDay dt
yearOf _ = infError

hourOf :: [Value] -> Interpreter EvalError Value
hourOf [VDate dt] = return . VNum . fromIntegral . todHour $ utcToTOD dt
hourOf _ = infError

minOf :: [Value] -> Interpreter EvalError Value
minOf [VDate dt] = return . VNum . fromIntegral . todMin $ utcToTOD dt
minOf _ = infError

utcToTOD :: UTCTime -> TimeOfDay
utcToTOD = timeToTimeOfDay . utctDayTime

now :: [Value] -> Interpreter EvalError Value
now [] = return . VDate $ unsafePerformIO getCurrentTime
now _  = infError

monthName :: [Value] -> Interpreter EvalError Value
monthName [VDate dt] = return . VText $ formatTime defaultTimeLocale "%B" dt
monthName _  = infError

quarter :: [Value] -> Interpreter EvalError Value
quarter [VDate dt] = return . VNum $ fromIntegral qt
  where
    qt = (mo + 2) `div` 3
    (_,mo,_) = toGregorian (utctDay dt)
quarter _  = infError

weekDay :: [Value] -> Interpreter EvalError Value
weekDay [VDate dt] = return . VText $ formatTime defaultTimeLocale "%A" dt
weekDay _  = infError

builtInDate :: M.Map String BuiltIn
builtInDate = M.fromList
    [ ("dateadd", BuiltIn dateAdd "DATEADD" (typeTimeUnit :-> typeNum :-> typeDate :-> typeDate))
    , ("datedif", BuiltIn dateDif "DATEDIF" (typeDate :-> typeDate :-> typeTimeUnit :-> typeNum))
    , ("datetimevalue", BuiltIn dateTimeVal "DATETIMEVALUE" (typeDate :-> typeNum))
    , ("day",      BuiltIn dayOf     "DAY"       (typeDate :-> typeNum))
    , ("hour",     BuiltIn hourOf    "HOUR"      (typeDate :-> typeNum))
    , ("minute",   BuiltIn minOf     "MINUTE"    (typeDate :-> typeNum))
    , ("month",    BuiltIn monthOf   "MONTH"     (typeDate :-> typeNum))
    , ("monthname",BuiltIn monthName "MONTHNAME" (typeDate :-> typeText))
    , ("now",      BuiltIn now       "NOW"        typeDate)
    , ("quarter",  BuiltIn quarter   "QUARTER"   (typeDate :-> typeNum))
    , ("weekday",  BuiltIn weekDay   "WEEKDAY"   (typeDate :-> typeText))
    , ("year",     BuiltIn yearOf    "YEAR"      (typeDate :-> typeNum))
    ]

helpDate :: M.Map String String
helpDate = M.fromList
    [ ("dateadd", "dateadd datetime_unit increment datetime;")
    , ("datedif", "datedif start_date end_date datetime_unit;")
    ]
