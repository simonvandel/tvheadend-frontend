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
  -> Event t Channel -- ^ Channel to display.
  -> m () -- ^ Player widget
player tvhBaseUrl selectedChannelDyn = do
  -- If no channel is selected yet, then set the src to an empty string
  videoSrcDyn <- holdDyn "" $ ffor selectedChannelDyn streamUrl

  void $ videoEl videoSrcDyn
  where
    streamUrl chan = tvhBaseUrl <> "stream/channel/" <> (_cid chan)

videoEl :: MonadWidget t m
  => Dynamic t String -- ^ Video source
  -> m () -- ^ Video element
videoEl srcDyn = do
  attrs <- forDyn srcDyn $ \src ->
    "src"      =: src <>
    "controls" =: "" <>
    "autoplay" =: "" <>
    "style"    =: "width:100%;height:auto;max-height:100%"
  elDynAttr "video" attrs blank
