module Utilities
    (
    nowAndPeriodically
    ) where

import           Reflex.Dom
import           Data.Time.Clock
import           Control.Monad.IO.Class

-- | Creates an event that fires on post build and from then on periodically at the specified interval
nowAndPeriodically :: MonadWidget t m
  => NominalDiffTime -- ^ Time interval
  -> m (Event t ())
nowAndPeriodically df = do
  -- Get the event for 'now'
  nowEvent <- getPostBuild
  utcNow <- liftIO $ getCurrentTime
  -- Create an event that fires periodically after the now event
  periodically <- return . tag (constant ()) =<< tickLossyFrom df utcNow nowEvent
  -- Combine the two events
  return $ leftmost [nowEvent,  periodically]
