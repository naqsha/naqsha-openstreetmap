{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE Rank2Types                 #-}

-- | The basic elements of open street map.
module Naqsha.OpenStreetMap.Element
       (

       -- * Open Street Map elements.
       -- $osm$
         Node, Way, Relation
       , NodeID, WayID, RelationID
       , Member(..)
       -- * Semantics of Elements.
       -- $semantics$
       , Location(..)
       , wayNodes, relationMembers
       , Tags, Tagged
       , Semantic(..)
       , tagAt
       ) where

import           Control.Monad                  ( liftM )
import           Control.Lens
import           Data.Default
import qualified Data.HashMap.Lazy           as HM
import           Data.Text   hiding             (empty)
import           Data.Time
import           Data.Word
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU

import Naqsha.Geometry
import Naqsha.OpenStreetMap.ID



-- $osm$
--
-- The Open street map describes the world using three kinds of
-- elements given by the types `Node`, `Way` and `Relation`
-- respectively. Intuitively, an element of type `Node` captures a
-- location, a `Way` captures path traversed on the globe, and a
-- relation is a collection of nodes and ways which together forms
-- some interesting cartographic entity.
--

-- $semantics$
--
-- The type of the element determine its basic semantics. For example,
-- a `Node` is a position on the globe and hence has a `Latitude` and
-- a `Longitude` associated with it. A way is an ordered list of nodes
-- that forms the continuous curve on the globe, and a relation is on
-- a collection of its members elements. Besides these basic semantic
-- content, elements often have other cartographic information
-- associated with them.  For example, a node might be a location of a
-- bus stop. A `Way` might be a road or a boundary of a region or a
-- river. Open Street Map associates all such information through a
-- collection of optional @(key,value)@ pairs called tags. The type
-- @`Tagged` elem@ captures the tagged version of an element of type
-- @ele@.
--
-- == Accessing Semantic data.
--
-- Naqsha provides /lenses/ to access and modify any of the above
-- sematic information associated with an object. For example, the
-- @`latitude`@ lens provides the latitude of any node. Standard tags
-- are exposed by the the module "Naqsha.OpenStreetMap.Tags". In case
-- a tag is not supported by the above module, one can use the `tagAt`
-- lens. However, beware that one looses a lot of type safety in the
-- process.
--
-- We also expose a lens based creation and updation interface using
-- the `build` and `withChanges` combinators.
--
--
-- > import Control.Lenses
-- > import Naqsha.OpenStreetMap
-- >
-- > kanpur :: Osm Node
-- > kanpur = build $ do latitude  .= lat 26.4477777  -- basic information
-- >                     longitude .= lon 80.3461111  -- basic information
-- >                     name      .= Just "Kanpur"   -- the name tag. Notice the Just
-- >
-- > -- The above definition could be used for Tagged Node as well as we are not setting any
-- > -- database fields.
-- >
-- >
-- > kanpurHindi = kanpur `withChanges` do
-- >       nameIn hindi .= Just "कानपुर" -- adds a multi-lingual name.
-- >
-- > -- Unsets the elevation tag of  node
-- > unsetElevation :: Osm Node -> Osm Node
-- > unsetElevation x = x `withChanges` do elevation .= Nothing
--
-- Notice that all lenses corresponding to tags focus on a value of
-- type @`Maybe` v@. This is because, all tags in Open Street Map are
-- optional and can be set/unset using `Just` and `Nothing`
-- respectively.


-- | Types that are essentially locations on the globe. They are
-- associated with three lenses, latitude, longitude and
-- `Geo`-position.

class Location a where
  -- | The latitude of the object.
  latitude    :: Lens' a Latitude

  -- | The longitude of the object.
  longitude   :: Lens' a Longitude

  -- | The geo-Position of the object.
  geoPosition :: Lens' a Geo

  latitude = geoPosition . latitude

  longitude = geoPosition . longitude


instance Location Geo where
  latitude    = lens (\ (Geo lt _) -> lt) $ \ (Geo _ lo) lt -> Geo lt lo
  longitude   = lens (\ (Geo _ lo) -> lo) $ \ (Geo lt _) lo -> Geo lt lo
  geoPosition = lens id $ flip const


------------------------- Semantic Tags ---------------------------------

-- | The tags of an OSM element.
type Tags = HM.HashMap Text Text

-- | Element @elem@ glued with its tags.
data Tagged elem = Tagged { __element :: elem
                          , __tags    :: Tags
                          } deriving (Show, Eq)

makeLenses ''Tagged

-- | The default value is with empty tags.
instance Default e => Default (Tagged e) where
  def = Tagged def HM.empty

instance Location e => Location (Tagged e) where
  latitude    = _element . latitude
  longitude   = _element . longitude
  geoPosition = _element . geoPosition


-- | Family of types that have Open street map tags.
class Semantic elem where

  -- | The type family that captures the underlying element.
  type ElementType elem :: *

  -- | Lens to focus on the tags
  tags     :: Lens' elem Tags

  -- | Lens to focus on the untaged element.
  untagged :: Lens' elem (ElementType elem)

-- | The untagged element type in e
instance Semantic (Tagged e) where
  type ElementType (Tagged e) = e
  tags     = _tags
  untagged = _element


-------------- Some useful lenses -------------------------------

-- | Lens to focus on the tag at a given key. A more type safe method
-- to access tags is given in the module
-- "Naqsha.OpenStreetMap.Tags". Always prefer the one exposed from
-- this module instead of using `tagAt` directly.
tagAt :: Semantic a => Text -> Lens' a (Maybe Text)
tagAt k = tags . at k


---------- The element of Open street map -----------------

-- | An OpenStreetMap node is nothing but a geo-position.
type    Node       = Geo

-- | ID of Nodes
type    NodeID     = ID Node



-- | The primitive way type.
newtype Way  = Way       { __wayNodes        :: VU.Vector NodeID } deriving (Show, Eq)
-- | ID of Ways
type    WayID      = ID Way


instance Default Way where
  def = Way VU.empty
-- | A member of a relation.

data Member = NodeM     Text NodeID
            | WayM      Text WayID
            | RelationM Text RelationID deriving (Show, Eq)

-- | The primitive relation type.
newtype Relation = Relation { __relationMembers :: V.Vector Member } deriving (Show, Eq)
-- | ID of Relations
type    RelationID = ID Relation

makeLenses ''Way
makeLenses ''Relation

-- | Lens to focus on the node ids in the way.
wayNodes :: Lens' Way (VU.Vector NodeID)
wayNodes =  _wayNodes

-- | Lens to focus on the members of a relation.
relationMembers  :: Lens' Relation (V.Vector Member)
relationMembers = _relationMembers
