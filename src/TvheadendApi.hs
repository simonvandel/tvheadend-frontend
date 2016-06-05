module TvheadendApi
    (
    tvhChannelsReq
    ) where

import Reflex.Dom
import Data.Monoid

tvhChannelsReq :: String -- Base Tvheadend url
            -> XhrRequest -- The request
tvhChannelsReq tvhBaseUrl =
  xhrRequest "GET" url config
  where
    url = tvhBaseUrl <> "/api/channel/list"
    config = def-- { _xhrRequestConfig_headers = headers}
    headers = "withCredentials" =: "true"
