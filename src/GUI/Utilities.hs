module GUI.Utilities
    (
    clickableListItem
    ) where

import           Reflex.Dom
import           Data.Monoid

-- | Widget to be placed as an item in a list.
--   The item can be associated with some data.
--   Whenever the item is clicked, it will fire an event.
clickableListItem :: MonadWidget t m
  => Dynamic t a   -- ^ Data associated with item
  -> Dynamic t Bool -- ^ Bool indicating whether the item is selected in a list
  -> m b           -- ^ Child widget
  -> m (Event t a) -- ^ MDL widget that sends click events
clickableListItem assocDataDyn selectedDyn child = do
  -- Determine whether the widget should display itself as active
  activeness <- mapDyn (\selected -> if selected
    then "active" :: String
    else "") selectedDyn

  attrs <- forDyn (traceDyn "asd" activeness) (\active -> "class" =: ("list-group-item " <> active) <> "href" =: "#")

  (element, _) <- elDynAttr' "a" attrs child
  -- TODO: Because we are sampling the dynamic, there might be some issues in the data not updating.
  -- Extract the associated data from the dynamic
  assocData <- sample . current $ assocDataDyn
  -- Return the click event tagged with the channel corresponding to the nav item
  let clickEvent = domEvent Click element
  return $ (const assocData) <$> clickEvent
