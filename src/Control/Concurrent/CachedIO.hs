module Control.Concurrent.CachedIO (
    cachedIO
    ) where

import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar)
import Control.Monad (join)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)

-- | Cache an IO action, producing a version of this IO action that is cached
-- for 'interval' seconds. Immediately initialize the cache with the given IO
-- action.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh, if the cache is older than 'interval'
-- seconds.
cachedIO :: NominalDiffTime -> IO a -> IO (IO a)
cachedIO interval io = do
  initValue <- io
  initTime <- getCurrentTime
  cachedT <- atomically (newTVar (initTime, initValue))
  return $ do
    now <- getCurrentTime
    join . atomically $ do
      cached <- readTVar cachedT
      case cached of
        (lastUpdated, value) | addUTCTime interval lastUpdated > now ->
          return (return value)
        (_, value) -> do
          writeTVar cachedT (now, value)
          return $ do
            newValue <- io
            atomically (writeTVar cachedT (now, newValue))
            return newValue
