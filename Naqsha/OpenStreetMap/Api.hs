{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Naqsha.OpenStreetMap.Api
  ( -- * The OpenStreetMap API
    OSMApi
  , module Naqsha.OpenStreetMap.Api.Info
  , module Naqsha.OpenStreetMap.Api.ContentTypes
  ) where

import Naqsha.OpenStreetMap.Api.Info
import Naqsha.OpenStreetMap.Api.ContentTypes
import Servant.API

type OSMApi  = "api" :> "capabilities" :> Get '[XML, XMLPretty] Api
