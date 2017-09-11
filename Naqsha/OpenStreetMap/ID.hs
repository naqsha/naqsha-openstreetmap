{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


-- | Module that captures id's of objects.
module Naqsha.OpenStreetMap.ID
  ( ID, unsafeToID
  ) where

import           Control.Monad                  ( liftM )
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Data.Vector.Unboxed            (Vector, MVector, Unbox)
import           Data.Word                      ( Word64 )

-- | The ID of an object in open street map. Currently, the Open
-- Street map project uses 64-bit word for ids. We use the phantom
-- type of the entity for better type safety.
newtype ID element  = ID Word64 deriving (Eq, Ord)

-- | Unsafe conversion of Word64 into an OsmID. You will hardly need
-- this except for some very special circumstances. The type safety is
-- due to this.
unsafeToID :: Word64 -> ID e
unsafeToID = ID

instance Show (ID element) where
  show (ID x) = show x

instance Unbox (ID element)

newtype instance MVector s (ID element) = MIDV  (MVector s Word64)
newtype instance Vector    (ID element) = IDV   (Vector Word64)

instance GVM.MVector MVector (ID element) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          (MIDV v)          = GVM.basicLength v
  basicUnsafeSlice i n (MIDV v)          = MIDV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MIDV v1) (MIDV v2)     = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MIDV v) i            = ID `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MIDV v) i (ID x)  = GVM.basicUnsafeWrite v i x

  basicClear (MIDV v)                    = GVM.basicClear v
  basicSet   (MIDV v)         (ID x)  = GVM.basicSet v x

  basicUnsafeNew n                        = MIDV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (ID x)    = MIDV `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MIDV v1) (MIDV v2)   = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MIDV v)   n           = MIDV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MIDV v)               = GVM.basicInitialize v
#endif

instance GV.Vector Vector (ID element) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MIDV v)         = IDV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (IDV v)            = MIDV `liftM` GV.basicUnsafeThaw v
  basicLength (IDV v)                = GV.basicLength v
  basicUnsafeSlice i n (IDV v)       = IDV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (IDV v) i        = ID   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MIDV mv) (IDV v) = GV.basicUnsafeCopy mv v
  elemseq _ (ID x)                 = GV.elemseq (undefined :: Vector a) x
