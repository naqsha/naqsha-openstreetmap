{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The OpenStreetMap database keeps track of certain meta
-- information for each element. This module gives access to these
-- informations.
module Naqsha.OpenStreetMap.Database
      (
        -- * The database element.
        Element
       -- ** Lenses to access the meta data.
       --
       , elementID, modifiedUser, modifiedUserID, timeStamp, changeSet, version
       , isVisible, unMeta, meta

       -- ** The metadata type.
       , Meta
       , _elementID, _modifiedUser, _modifiedUserID, _timeStamp, _changeSet, _version
       , _isVisible
      ) where

import Control.Lens
import Data.Default
import Data.Text
import Data.Time
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.ID

-- | The open street map metadata that is associated with each
-- element.
data Meta a = Meta { __elementID      :: Maybe (ID a)
                   , __modifiedUser   :: Maybe Text
                   , __modifiedUserID :: Maybe Integer
                   , __isVisible      :: Maybe Bool
                   , __version        :: Maybe Integer
                   , __timeStamp      :: Maybe UTCTime
                   , __changeSet      :: Maybe Integer
                   } deriving (Show, Eq)


makeLenses ''Meta

-- | The database element associated with the element type @elem@.
data Element elem = Element { __element  :: Tagged elem
                            , __meta     :: Meta elem
                            } deriving (Show, Eq)

makeLenses ''Element


instance Default e => Default (Element e) where
  def = Element def def

-- | The associated untagged element is e.
instance Semantic (Element elem) where
  type ElementType (Element elem) = elem
  tags     = _element . tags
  untagged = _element . untagged

instance Location e => Location (Element e) where
  latitude    = untagged . latitude
  longitude   = untagged . longitude
  geoPosition = untagged . geoPosition


instance Default (Meta a) where
  def = Meta { __elementID      = Nothing
             , __modifiedUser   = Nothing
             , __modifiedUserID = Nothing
             , __isVisible      = Nothing
             , __version        = Nothing
             , __timeStamp      = Nothing
             , __changeSet      = Nothing
             }

-- | Strip off the meta data and return the underlying tagged data.
unMeta :: Lens' (Element e) (Tagged e)
unMeta = _element

-- | Lens to focus on the meta information of the element.
meta :: Lens' (Element e) (Meta e)
meta = _meta


-- | Lens to focus on the Id of the element.
elementID :: Lens' (Element a) (Maybe (ID  a))
elementID = _meta . _elementID

-- | Lens to focus on the user who last modified.
modifiedUser  :: Lens' (Element a) (Maybe Text)
{-# INLINE modifiedUser #-}
modifiedUser = _meta . _modifiedUser

-- | Lens to focus on the user id of the user that last modified.
modifiedUserID :: Lens' (Element a) (Maybe Integer)
modifiedUserID = _meta . _modifiedUserID

-- | Flag which indicates whether the associated element is visible or
-- not.
isVisible :: Lens' (Element a) (Maybe Bool)
isVisible  = _meta . _isVisible

-- | The version number of the associated entry.
version :: Lens' (Element a) (Maybe Integer)
version =  _meta . _version

-- | The time stamp (utc) when the entry was last changed.
timeStamp :: Lens' (Element a) (Maybe UTCTime)
timeStamp = _meta . _timeStamp

-- | The change set number where the object was changed.
changeSet :: Lens' (Element a) (Maybe Integer)
changeSet = _meta . _changeSet
