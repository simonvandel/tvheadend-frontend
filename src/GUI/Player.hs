module GUI.Player
    (
    player
    ) where

import           Models.Channel
import           Reflex.Dom
import           GUI.Utilities
import           Data.Monoid

player :: MonadWidget t m
  => String -- ^ Base url to tvheadend server
  -> Dynamic t (Maybe Channel) -- ^ Channel to display.
  -> m () -- ^ Player widget
player tvhBaseUrl selectedChannelDyn = do
  --selectedChannel <- sample . current $ selectedChannelDyn
  -- _ <-
  --   forDyn selectedChannelDyn $ \(selectedChannel::Maybe Channel) ->
  --     case selectedChannel of
  --       Nothing -> _--text "No channel is currently selected"
  --       Just channel -> undefined

  --_ <- widgetHold initialWidget firedWidget

  x <- forDyn selectedChannelDyn $ \(channelMay :: Maybe Channel) ->
          case channelMay of
            Nothing -> text "asd"
            Just channel -> do
              el "h1" $ text (_cname channel)
              let src = tvhBaseUrl <> "stream/channel/" <> (_cid channel)  --"http://v2v.cc/~j/theora_testsuite/320x240.ogg"
              videoEl src

  dyn x

  return ()
  -- where
  --   -- This widget is used when the event in the channel dynamic has not fired yet, i.e. no channel has been selected yet
  --   --initialWidget :: MonadWidget t m => m ()
  --   initialWidget = text "No channel is currently selected"
  --   -- This widget is used and recreated whenever the channel dynamic fires
  --   --firedWidget :: MonadWidget t m => Event t (m ())
  --   firedWidget =
  --     ffor (updated selectedChannelDyn) $ \(channelMay :: Maybe Channel) ->
  --       case channelMay of
  --         Nothing -> text "asd"
  --         Just channel -> do
  --           el "h1" $ text (_cname channel)
  --           let src = tvhBaseUrl <> "stream/channel/" <> (_cid channel)  --"http://v2v.cc/~j/theora_testsuite/320x240.ogg"
  --           videoEl src
