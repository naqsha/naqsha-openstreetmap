{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | API information
module Naqsha.OpenStreetMap.Api.Info
  ( version
  , Api
  , apiVersions
  , apiArea
  , apiTracePointsPerPage
  , apiMaximumWayNodes
  , apiMaximumChangeSetsElements
  , apiStatus
  , apiDatabaseStatus
  , apiGPXStatus
  , Status(..)
  , VersionRange
  , maxVersion
  , minVersion
  ) where

import Control.Lens
import Data.Default
import Data.Text                     as Text
import Data.Version
import Text.XML
import Naqsha.OpenStreetMap.Internal
import Naqsha.OpenStreetMap.Api.XML

-- | The version of this API
version :: Version
version = Version [0,6][]

------------------------  API capabilities information ---------------------------
data Status       = Online | ReadOnly | Offline deriving Show
data VersionRange = VersionRange { _minVersion :: Version
                                 , _maxVersion :: Version
                                 }

makeLenses ''VersionRange
instance ToXML VersionRange where
  toXML VersionRange{..} = "version" `withAttrs` [ ("minimum", showVersionT _minVersion)
                                                 , ("maximum", showVersionT _maxVersion)
                                                 ]



data Api
  = Api { _apiVersions                   :: VersionRange
        , _apiArea                       :: Double -- ^ Maximum square degrees supported.
        , _apiTracePointsPerPage         :: Integer
        , _apiMaximumWayNodes            :: Integer
        , _apiMaximumChangeSetsElements  :: Integer
        , _apiTimeOut                    :: Integer
        , _apiStatus                     :: Status
        , _apiDatabaseStatus             :: Status
        , _apiGPXStatus                  :: Status
        }

instance Default Api where
  def = Api { _apiVersions           = VersionRange version version
            , _apiArea               = 0
            , _apiTracePointsPerPage = 0
            , _apiMaximumWayNodes    = 0
            , _apiMaximumChangeSetsElements = 0
            , _apiTimeOut                   = 0
            , _apiDatabaseStatus = Offline
            , _apiStatus         = Offline
            , _apiGPXStatus      = Offline
            }

makeLenses ''Api

instance ToXML Api where
  toXML Api{..} = "api" `withBody` [ toXML _apiVersions
                                   , area
                                   , tracePoints
                                   , wayNodes
                                   , changeSets
                                   , timeOut
                                   , status
                                   ]
    where area        = "area"        `withAttr`  ("maximum"         , showT      _apiArea)
          tracePoints = "tracepoints" `withAttr`  ("per_page"        , showT      _apiTracePointsPerPage)
          wayNodes    = "waynodes"    `withAttr`  ("maximum"         , showT      _apiMaximumWayNodes)
          changeSets  = "changesets"  `withAttr`  ("maximum_elements", showT      _apiMaximumChangeSetsElements)
          timeOut     = "timeout"     `withAttr`  ("seconds"         , showT      _apiTimeOut)
          status      = "status"      `withAttrs` [ ("api"           , statusText _apiStatus)
                                                  , ("database"      , statusText _apiDatabaseStatus)
                                                  , ("gpx"           , statusText _apiGPXStatus)
                                                  ]
          statusText  = Text.toLower . showT
