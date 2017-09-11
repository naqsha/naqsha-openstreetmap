module Naqsha.OpenStreetMap.Api.XML
  ( ToXML(..), withBody, withAttrs, withAttr
  ) where

import qualified Data.Map  as Map
import           Data.Text        ( Text )
import           Text.XML


class ToXML a where
  toXML :: a -> Element


-- | Element which only has a body.
withBody :: Name -> [Element] -> Element
withBody n = Element n Map.empty . map NodeElement

-- | Element which only has attributes.
withAttrs :: Name -> [(Name,Text)] -> Element
withAttrs n attrs = Element n (Map.fromList attrs) []

withAttr :: Name -> (Name,Text) -> Element
withAttr n a = withAttrs n [a]
