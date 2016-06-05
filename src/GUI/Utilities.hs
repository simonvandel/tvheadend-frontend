module GUI.Utilities
    (
    matElClass',
    matElClass,
    matDivClass,
    mdlNavigationItem
    ) where

import           Reflex.Dom
import           GHCJS.DOM.Types (unElement)
import           GHCJS.DOM.Element (toElement)
import           GHCJS.Types
import           Control.Monad.IO.Class
import           Data.Monoid

-- http://www.getmdl.io/started/index.html#dynamic
-- As we are adding elements to the DOM after the page has loaded,
-- we need to tell MDL (Material Design Lite) about the elements.
foreign import javascript unsafe
  "if (typeof componentHandler != 'undefined') {\
    \componentHandler.upgradeElement($1);\
   \}"
  materialInitJS :: JSVal -> IO ()

-- TODO: I do not know if it is correct to just return the value
matElClass' :: MonadWidget t m
  => String      -- ^ Element name
  -> String      -- ^ Classes for element
  -> m a         -- ^ Child element
  -> m (El t, a) -- ^ Widget with it's underlying element
matElClass' elName classes child  = do
  (reflexEl, value') <- elAttr' elName
                       ("class" =: classes) $ child
  let jsel = unElement $ toElement $ _el_element reflexEl
  liftIO $ materialInitJS jsel
  return (reflexEl, value')

matElClass :: MonadWidget t m
  => String -- ^ Element name
  -> String -- ^ Classes for element
  -> m a    -- ^ Child element
  -> m a    -- ^ Widget
matElClass elName classes child = fmap snd $ matElClass' elName classes child

matDivClass :: MonadWidget t m => String -> m a -> m a
matDivClass classes = matElClass "div" classes

-- TODO: set cursor to clickable
mdlNavigationItem :: MonadWidget t m
  => Dynamic t a   -- ^ Data associated with item
  -> m b           -- ^ Child widget
  -> m (Event t a) -- ^ MDL widget that sends click events
mdlNavigationItem assocDataDyn child = do
  (element, _) <- matElClass' "a" "mdl-navigation__link" child
  -- TODO: Because we are sampling the dynamic, there might be some issues in the data not updating.
  -- Extract the associated data from the dynamic
  assocData <- sample . current $ assocDataDyn
  -- Return the click event tagged with the channel corresponding to the nav item
  return $ (const assocData) <$> (domEvent Click element)
