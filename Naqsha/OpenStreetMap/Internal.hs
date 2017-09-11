{-# LANGUAGE OverloadedStrings #-}
-- | Some common function that is required every where.
module Naqsha.OpenStreetMap.Internal
       ( showT, readMaybeT, showVersionT, naqshaVersionT
       , showTime, timeParser, osmXmlVersion
       ) where

import Data.Monoid
import Data.Text       ( Text, pack, unpack       )
import Data.Time
import Data.Version    ( Version(..), showVersion )
import Text.Read       ( readMaybe                )
import Paths_naqsha_openstreetmap as NaqshaOSM


-- | Time format used by osm.
osmDisplayFmt :: String
osmDisplayFmt = "%Y-%m-%dT%T%Q%z"

-- | Time parse format (more liberal than display format).
osmParseFmt :: String
osmParseFmt = "%Y-%m-%dT%T%Q%Z"


-- | The xml version of osm.
osmXmlVersion   :: Version
osmXmlVersion   = Version [0,6] []

-- | show the time as text
showTime :: FormatTime t => t -> Text
showTime = pack . formatTime defaultTimeLocale osmDisplayFmt

-- | read teh time as text
timeParser :: (Monad m, ParseTime t) => Text -> m t
timeParser = parseTimeM True defaultTimeLocale osmParseFmt . unpack


-- | Text variants of show
showT :: Show a => a -> Text
showT = pack . show

-- | Text variant of read.
readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . unpack


-- | Text variant of show version
showVersionT :: Version -> Text
showVersionT = pack . showVersion

-- | The version text for the library.
naqshaVersionT :: Text
naqshaVersionT = "naqsha-" <> showVersionT NaqshaOSM.version
