{-# LANGUAGE OverloadedStrings #-}
module BuiltIn.Date (builtInDate) where

-- import           Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Time as T
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           System.IO.Unsafe

import           Syntax
import           Type
import           Types

infError :: Interpreter EvalError Value
infError = error $ "Received unexpected types. Either the built-in was " ++
                   "improperly defined or type inference failed."

biErr :: String -> Interpreter EvalError Value
biErr = throwError . EvBuiltInError

biDateAdd :: BuiltIn
biDateAdd = BuiltIn
    { evalVal = dateAdd
    , emitVal = const "DATEADD"
    , typeSig = typeTimeUnit :-> typeNum :-> typeDate :-> typeDate
    , argHelp = "datetime_unit increment datetime"
    , addHelp = Nothing
    }

dateAdd :: [Value] -> Interpreter EvalError Value
dateAdd [VTimUn unit, VNum intv, VDate dt] =
    case unit of
        Day  -> return $ VDate . secAdd $ realToFrac (86400*intv)
        Hour -> return . VDate . secAdd $ realToFrac (3600*intv)
        Min  -> return . VDate . secAdd $ realToFrac (60*intv)
  where
    secAdd x = T.addUTCTime x dt
dateAdd _ = infError

biDateDif :: BuiltIn
biDateDif = BuiltIn
    { evalVal = dateDif
    , emitVal = const "DATEDIF"
    , typeSig = typeDate :-> typeDate :-> typeTimeUnit :-> typeNum
    , argHelp = "start_date end_date datetime_unit"
    , addHelp = Nothing
    }

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
    secs = floor $ toRational $ T.diffUTCTime end st
dateDif _  = infError

dateTimeVal :: [Value] -> Interpreter EvalError Value
dateTimeVal [VDate dt] = return $ VNum $ realToFrac
                                         (T.diffUTCTime dt start) / 86400
  where
    start = T.UTCTime (T.fromGregorian 1900 01 01) 0
dateTimeVal _  = infError

biDayOf :: BuiltIn
biDayOf = BuiltIn
    { evalVal = dayOf
    , emitVal = const "DAY"
    , typeSig = typeDate :-> typeNum
    , argHelp = "date"
    , addHelp = Nothing
    }

dayOf :: [Value] -> Interpreter EvalError Value
dayOf [VDate dt] = return . VNum $ fromIntegral d
  where
    (_,_,d) = T.toGregorian $ T.utctDay dt
dayOf _ = infError

biMonthOf :: BuiltIn
biMonthOf = BuiltIn
    { evalVal = monthOf
    , emitVal = const "MONTH"
    , typeSig = typeDate :-> typeNum
    , argHelp = "date"
    , addHelp = Nothing
    }

monthOf :: [Value] -> Interpreter EvalError Value
monthOf [VDate dt] = return . VNum $ fromIntegral m
  where
    (_,m,_) = T.toGregorian $ T.utctDay dt
monthOf _ = infError

biYearOf :: BuiltIn
biYearOf = BuiltIn
    { evalVal = yearOf
    , emitVal = const "YEAR"
    , typeSig = typeDate :-> typeNum
    , argHelp = "date"
    , addHelp = Nothing
    }

yearOf :: [Value] -> Interpreter EvalError Value
yearOf [VDate dt] = return . VNum $ fromIntegral y
  where
    (y,_,_) = T.toGregorian $ T.utctDay dt
yearOf _ = infError

biHourOf :: BuiltIn
biHourOf = BuiltIn
    { evalVal = hourOf
    , emitVal = const "HOUR"
    , typeSig = typeDate :-> typeNum
    , argHelp = "date"
    , addHelp = Nothing
    }

hourOf :: [Value] -> Interpreter EvalError Value
hourOf [VDate dt] = return . VNum . fromIntegral . T.todHour $ utcToTOD dt
hourOf _ = infError

biMinOf :: BuiltIn
biMinOf = BuiltIn
    { evalVal = minOf
    , emitVal = const "MINUTE"
    , typeSig = typeDate :-> typeNum
    , argHelp = "date"
    , addHelp = Nothing
    }

minOf :: [Value] -> Interpreter EvalError Value
minOf [VDate dt] = return . VNum . fromIntegral . T.todMin $ utcToTOD dt
minOf _ = infError

utcToTOD :: T.UTCTime -> T.TimeOfDay
utcToTOD = T.timeToTimeOfDay . T.utctDayTime

biNow :: BuiltIn
biNow = BuiltIn
    { evalVal = now
    , emitVal = const "NOW"
    , typeSig = typeDate
    , argHelp = ""
    , addHelp = Nothing
    }

now :: [Value] -> Interpreter EvalError Value
now [] = return . VDate $ unsafePerformIO T.getCurrentTime
now _  = infError

biMonthName :: BuiltIn
biMonthName = BuiltIn
    { evalVal = monthName
    , emitVal = const "MONTHNAME"
    , typeSig = typeDate :-> typeText
    , argHelp = "date"
    , addHelp = Nothing
    }

monthName :: [Value] -> Interpreter EvalError Value
monthName [VDate dt] = return . VText $ T.formatTime defaultTimeLocale "%B" dt
monthName _  = infError

biQuarter :: BuiltIn
biQuarter = BuiltIn
    { evalVal = quarter
    , emitVal = const "QUARTER"
    , typeSig = typeDate :-> typeNum
    , argHelp = "date"
    , addHelp = Nothing
    }

quarter :: [Value] -> Interpreter EvalError Value
quarter [VDate dt] = return . VNum $ fromIntegral qt
  where
    qt = (mo + 2) `div` 3
    (_,mo,_) = T.toGregorian (T.utctDay dt)
quarter _  = infError

biWeekDay :: BuiltIn
biWeekDay = BuiltIn
    { evalVal = weekDay
    , emitVal = const "WEEKDAY"
    , typeSig = typeDate :-> typeText
    , argHelp = "date"
    , addHelp = Nothing
    }

weekDay :: [Value] -> Interpreter EvalError Value
weekDay [VDate dt] = return . VText $ T.formatTime defaultTimeLocale "%A" dt
weekDay _  = infError

builtInDate :: M.Map String BuiltIn
builtInDate = M.fromList
    [ ("dateadd",       biDateAdd)
    , ("datedif",       biDateDif)
    , ("datetimevalue", biDateTimeVal)
    , ("day",           biDayOf)
    , ("hour",          biHourOf)
    , ("minute",        biMinOf)
    , ("month",         biMonthOf)
    , ("monthname",     biMonthName)
    , ("now",           biNow)
    , ("quarter",       biQuarter)
    , ("weekday",       biWeekDay)
    , ("year",          biYearOf)
    ]
