{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import           Reflex.Dom
import           Data.JSString ()
import           GHCJS.Types
import           Models.Channel
import           GUI.ChannelDrawer
import           GUI.Player
import           GUI.Utilities

someFunc :: IO ()
someFunc = mainWidget mainUI

-- Needed until Reflex.Dom version 0.4 is released
foreign import javascript unsafe
  "$1.withCredentials = true;"
  setRequestWithCredentials :: JSVal -> IO ()

tvhBaseUrl = "http://localhost:9981/"

mainUI :: MonadWidget t m => m ()
mainUI =
  -- We need to have this outer main div, as MDL would otherwise try to access a
  -- parent that does not exist
  el "div" $ do
    matDivClass "mdl-layout mdl-js-layout mdl-layout--fixed-drawer" $ do
      curChannel :: Dynamic t (Maybe Channel) <- channelDrawer tvhBaseUrl
      matElClass "main" "mdl-layout__content" $ do
        player tvhBaseUrl curChannel
