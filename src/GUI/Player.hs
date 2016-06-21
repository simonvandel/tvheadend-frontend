module GUI.Player
    (
    player
    ) where

import           Models.Channel
import           Reflex.Dom
import           Data.Monoid
import           Control.Monad
import           GHCJS.Types
import           Control.Monad.IO.Class
import           GHCJS.DOM.Types (unElement)
import           GHCJS.DOM.Element (toElement)

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


foreign import javascript unsafe
  "if($1) {\
    \$2.style.cursor = 'none'\
  \} else {\
    \$2.style.cursor = 'auto'\
  \}"
  hideCursor :: Bool -> JSVal  -> IO ()

videoEl :: forall t m. MonadWidget t m
  => Dynamic t String -- ^ Video source
  -> m () -- ^ Video element
videoEl srcDyn = do
  attrs <- forDyn srcDyn $ \src ->
    "src"      =: src <>
    "controls" =: "" <>
    "autoplay" =: "" <>
    "style"    =: "width:100%;height:auto;max-height:100%"
  (element, _) <- elDynAttr' "video" attrs blank
  -- register event for mousemove, to hide cursor on inactivity
  let mouseActivity :: Event t Bool = fmap (const False) $ domEvent Mousemove element
  -- Wait for 3 seconds without a mousemove event before firing a mouseInactivity
  mouseInactivity :: Event t Bool <- fmap (fmap (const True)) $ debounce 3 mouseActivity
  -- Initially, there is not inactivity. Thereafter, there is either mouse activity or mouse inactivity
  inactivity :: Dynamic t Bool <- holdDyn False $ leftmost [mouseActivity, mouseInactivity]
  performEvent_ $ fmap (\inactive -> liftIO $ hideCursor inactive (unElement $ toElement $ _el_element element)) (updated inactivity)
  return ()
