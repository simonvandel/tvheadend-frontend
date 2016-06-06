module GUI.ChannelDrawer
    (
    channelDrawer
    ) where

import           TvheadendApi
import           Models.Channel
import           Reflex.Dom
import           Data.Text
import           GUI.Utilities
import           Utilities
import           Data.Time.Clock
import           Control.Monad.IO.Class

channelDrawer :: forall t m. MonadWidget t m
  => String -- ^ Base url to tvheadend server
  -> m (Dynamic t (Maybe Channel)) -- ^ Channel drawer that emits the value of the currently selected channel. The channel is wrapped in a Maybe type to cover the case that no channel is selected
channelDrawer tvhBaseUrl =
  matDivClass "mdl-layout__drawer" $ do
    matElClass "span" "mdl-layout-title" $ text "Channels"
    matDivClass "mdl-layout-spacer" blank
    matElClass "nav" "mdl-navigation" $ do
      -- Create stream of events that fire now and then periodically,
      -- signifying when to synchronize with the tvheadend server
      tickEvent <- nowAndPeriodically 10

      -- Replace the tick info with the actual XHR request
      let tvhChannelEvent :: Event t XhrRequest = tag (constant (tvhChannelsReq tvhBaseUrl)) tickEvent
      channelsJsonEvent :: Event t Text <- fmap (fmapMaybe getChannels) $ performRequestAsync $ tvhChannelEvent

      -- Parse the channels from JSON.
      -- Note that unparseable JSON is silently discared.
      -- TODO: Potentially display to the user that something went wrong in the JSON parsing.
      let parsedChannels :: Event t [Channel] = fmapMaybe parseChannelsJson channelsJsonEvent

      -- Make the parsed channels into a dynamic.
      -- Initially the list of channels should be empty.
      channels :: Dynamic t [Channel] <- holdDyn [] parsedChannels

      -- Display the channels in a list, showing only the channel name.
      channelClickEventsDyn :: Dynamic t [Event t Channel] <- simpleList channels (\channel ->
        mdlNavigationItem channel
          -- extract the name of the channel, and display it
          (dynText =<< mapDyn _cname channel))

      selectedChannel :: Event t (Maybe Channel) <-
        -- Remove the outer layer of Dynamic, leaving only Event
        return . switchPromptlyDyn =<<
        -- Merge all click events as one event, and wrap the Channel in Just,
        -- as it is sure that a channel is selected on-click.
        forDyn channelClickEventsDyn (fmap Just . leftmost)

      holdDyn Nothing selectedChannel

getChannels :: XhrResponse -> Maybe Text
getChannels xhrResponse =
  _xhrResponse_responseText xhrResponse
