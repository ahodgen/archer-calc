module EvalState
    ( Est
    , eStateInit
    , eStateUpdate
    ) where

import Data.IORef
import Data.Time (getCurrentTime, UTCTime(..))

newtype Est = Est (IORef EState)

data EState = EState
    { date       :: UTCTime
    , upDate     :: UTCTime -> IO UTCTime
    , trackingId :: Int
    , upTrack    :: Int -> IO Int
    , contentId  :: Int
    , upContent  :: Int -> IO Int
    }

eStateInit :: IO Est
eStateInit = do
    now <- getCurrentTime
    let initEState = EState
            { date       = now
            , upDate     = const getCurrentTime
            , trackingId = 678904
            , upTrack    = mIncr
            , contentId  = 34
            , upContent  = mIncr
            }
    fmap Est $ newIORef initEState
  where
    mIncr x = return $ x + 1
eStateUpdate :: Est -> IO ()
eStateUpdate (Est x) = do
    s <- readIORef x
    s' <- modState s
    writeIORef x s'

modState :: EState -> IO EState
modState x = do
    upd <- upDate x (date x)
    tid <- upTrack x (trackingId x)
    cid <- upTrack x (contentId x)
    return x
        { date = upd
        , trackingId = tid
        , contentId  = cid
        }
