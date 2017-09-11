-- | Module that captures id's of objects.
module Naqsha.OpenStreetMap.ID
  ( ID
  )

-- | The ID of an object in open street map. Currently, the Open
-- Street map project uses 64-bit word for ids. We use the phantom
-- type of the entity for better type safety.
newtype ID element  = ID Word64 deriving (Eq, Ord)


-- | Read the OSM id from text.
readOsmID :: Text -> Maybe (OsmID a)
readOsmID = fmap OsmID . readMaybeT

-- | Unsafe conversion of Word64 into an OsmID. You will hardly need
-- this except for some very special circumstances. The type safety is
-- due to this.
unsafeToOsmID :: Word64 -> OsmID e
unsafeToOsmID = OsmID
