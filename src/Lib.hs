{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import           Reflex.Dom
import           Data.JSString ()
import           GHCJS.Types
import           GUI.ChannelDrawer
import           GUI.Player
import           Data.Monoid

someFunc :: IO ()
someFunc = mainWidget mainUI

-- Needed until Reflex.Dom version 0.4 is released
foreign import javascript unsafe
  "$1.withCredentials = true;"
  setRequestWithCredentials :: JSVal -> IO ()

tvhBaseUrl :: String
tvhBaseUrl = "http://localhost:9981/"

navBar :: MonadWidget t m => m ()
navBar = do
  elClass "nav" "navbar navbar-default navbar-static-top" $ do
    divClass "container-fluid" $ do
      divClass "navbar-header" $ do
        divClass "navbar-brand" $ do
          text "Tvheadend frontend"

mainUI :: MonadWidget t m => m ()
mainUI = do
  navBar

  divClass "container-fluid" $ do
    divClass "row" $ do
      curChannel <- elAttr "div"
        ("class" =: "col-lg-2" <>
        -- height:90vh to fit the list inside the viewport
        -- overflow-y:auto to get vertical scrollbar
        -- padding-right:0px to pull scrollbar closer to the list
         "style" =: "height:90vh;overflow-y:auto;padding-right:0px") $ do
        -- create the channel drawer and react to events fired when a channel is selected
        channelDrawer tvhBaseUrl
      divClass "col-lg-10" $ do
        player tvhBaseUrl curChannel
