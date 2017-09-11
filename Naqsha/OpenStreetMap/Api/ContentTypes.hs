{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Naqsha.OpenStreetMap.Api.ContentTypes
  ( -- * OSM Xml content types.
    XML, XMLPretty
  ) where

import Data.Map                     as Map
import Data.Text                       ( pack )
import Data.Version
import Text.XML
import Paths_naqsha_openstreetmap   as Paths
import Naqsha.OpenStreetMap.Internal
import Naqsha.OpenStreetMap.Api.Info as Info
import Naqsha.OpenStreetMap.Api.XML


import Network.HTTP.Media        ( (//), (/:) )
import Servant.API.ContentTypes

----------------------- The content types -----------------------


-- | The XML API content type used by OpenStreetMap. The generated xml
-- string is optimised for network traffic.
data XML

-- | Similar to XML but the rendered xml is prettier (more human
-- readable). Good for debugging.
data XMLPretty

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")


instance Accept XMLPretty where
  contentType _ = "text" // "xml" /: ("charset", "utf-8")


doc :: Element -> Document
doc elem = Document (Prologue [] Nothing []) elem []
osm :: ToXML elem => elem -> Document
osm elem = doc $ Element "osm" (Map.fromList [ ("version", showVersionT Info.version)
                                             , ("generator", versionStr)
                                             ]
                               ) [ NodeElement $ toXML elem]

  where versionStr = pack $ "naqsha-openstreetmap-" ++ showVersion Paths.version


instance ToXML a => MimeRender XML a where
  mimeRender _ = renderLBS def . osm


instance ToXML a => MimeRender XMLPretty a where
  mimeRender _ = renderLBS (def { rsPretty = True }) . osm
