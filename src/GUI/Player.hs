module GUI.Player
    (
    player
    ) where

import           Models.Channel
import           Reflex.Dom
import           Data.Monoid
import           Control.Monad

player :: MonadWidget t m
  => String -- ^ Base url to tvheadend server
  -> Dynamic t (Maybe Channel) -- ^ Channel to display.
  -> m () -- ^ Player widget
player tvhBaseUrl selectedChannelDyn = do
  playerSrcDyn <- forDyn selectedChannelDyn $ \(channelMay :: Maybe Channel) ->
    case channelMay of
      Nothing -> "asd"
      Just channel -> do
        tvhBaseUrl <> "stream/channel/" <> (_cid channel)

  void $ videoEl playerSrcDyn

videoEl :: MonadWidget t m
  => Dynamic t String -- ^ Video source
  -> m () -- ^ Video element
videoEl srcDyn = do
  attrs <- forDyn srcDyn $ \src ->
    ("src" =: src <>
    "controls" =: "" <>
    "autoplay" =: "")
  elDynAttr "video" attrs blank
