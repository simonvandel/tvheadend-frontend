{-# LANGUAGE RecursiveDo #-}
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
import           Data.Map
import           Data.Tuple.Extra

channelDrawer :: forall t m. MonadWidget t m
  => String -- ^ Base url to tvheadend server
  -> m (Event t Channel) -- ^ Channel drawer that exposes an event that fires everytime a channel is selected
channelDrawer tvhBaseUrl =
  elClass "ul" "list-group" $ do
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

    channelsMap :: Dynamic t (Map Channel Channel) <- forDyn (traceDyn "channels" channels) (\_channels -> fmap dupe _channels) >>=
                   mapDyn fromList

    -- Set the currently selected channel to a none existing channel,
    -- and then change that on every channel selection
    rec curSelected <- holdDyn (Channel "" "") selectedChannel

        -- Display the channels in a list, showing only the channel name
        selectedChannel :: Event t Channel <-
          selectViewListWithKey (traceDyn "initial" curSelected) channelsMap
            (\channel _ selectedDyn ->
              clickableListItem (constDyn channel) selectedDyn
                -- extract the name of the channel, and display it
                (text $_cname channel))
          -- We only want the actual item's event
          >>= return . fmap snd

    return selectedChannel

getChannels :: XhrResponse -> Maybe Text
getChannels xhrResponse =
  _xhrResponse_responseText xhrResponse
