{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Models.Channel
    (
    parseChannelsJson,
    Channel(..)
    ) where

import Data.Text
import Data.Text.Encoding
import Data.Aeson.Types
import Data.Aeson
import Data.ByteString.Lazy
import Control.Monad as M
import Data.Vector as V

data Channel = Channel {
  _cname :: String,
  _cid :: String
} deriving (Show)

parseChannelsJson :: Text -> Maybe [Channel]
parseChannelsJson text = decode (fromStrict $ encodeUtf8 text) >>= parseMaybe parser
  where
    parser :: Object -> Parser [Channel]
    parser toplevel = do
      entries :: Array <- toplevel .: "entries"
      channelsVector :: Vector Channel <- M.forM entries $
        (\(Object entry) -> do
          key <- entry .: "key"
          title <- entry .: "val"
          return $ Channel title key
        )
      return $ V.toList channelsVector
