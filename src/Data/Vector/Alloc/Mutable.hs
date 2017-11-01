{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Vector.Alloc.Mutable(
  -- * Mutable vectors of 'Storable' types with custom allocators
  Allocator(..), StdMalloc,
  MVector(..), IOVector, STVector, Storable,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, write, modify, swap,
  unsafeRead, unsafeWrite, unsafeModify, unsafeSwap,

  -- * Modifying vectors

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove,

  -- * Unsafe conversions
  unsafeCast,

  -- * Raw pointers
  unsafeFromForeignPtr, unsafeFromForeignPtr0,
  unsafeToForeignPtr,   unsafeToForeignPtr0,
  unsafeWith
) where

import Control.DeepSeq ( NFData )

import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Storable.Mutable as VSM

import Data.Coerce
import Data.Proxy
import Foreign.Marshal.Alloc
import Foreign.C
import Foreign.Storable
import Foreign.ForeignPtr
import GHC.ForeignPtr

import Control.Monad.Primitive

import GHC.Ptr (Ptr(..))

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail )

import Data.Typeable ( Typeable )

-- Data.Vector.Internal.Check is not needed
-- | Mutable 'Storable'-based vectors
newtype MVector alloc s a = MVector (VSM.MVector s a)
        deriving (Typeable, NFData)

type IOVector alloc = MVector alloc RealWorld
type STVector alloc s = MVector alloc s

class Allocator alloc where
  allocVector :: Storable a => Proxy alloc -> Int -> Int -> IO (ForeignPtr a)

data StdMalloc

foreign import ccall unsafe "stdlib.h aligned_alloc" aligned_alloc :: CSize -> CSize -> IO (Ptr a)

-- * Allocate using standard C @malloc@.
instance Allocator StdMalloc where
  {-# INLINE allocVector #-}
  allocVector _ totalSize align = do
    ptr <- aligned_alloc (fromIntegral align) (fromIntegral totalSize)
    newForeignPtr finalizerFree ptr

data PinnedAlloc

-- * Allocate using a Haskell pinned @ByteArray@, like @Data.Vector.Storable@.
instance Allocator PinnedAlloc where
  {-# INLINE allocVector #-}
  allocVector _ totalSize align = do
    mallocPlainForeignPtrAlignedBytes totalSize align

instance (Allocator alloc, Storable a) => G.MVector (MVector alloc) a where
  {-# INLINE basicLength #-}
  basicLength = coerce (G.basicLength @VSM.MVector @a)

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = coerce (G.basicUnsafeSlice @VSM.MVector @a)

  -- FIXME: this relies on non-portable pointer comparisons
  {-# INLINE basicOverlaps #-}
  basicOverlaps = coerce (G.basicOverlaps @VSM.MVector @a)

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Alloc.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Alloc.basicUnsafeNew: length too large: " ++ show n
    | otherwise = unsafePrimToPrim $ do
        fp <- allocVector (Proxy @alloc) (n * sizeOf dummy) (alignment dummy)
        return (unsafeFromForeignPtr0 fp n)
    where
      dummy = undefined :: a
      mx = maxBound `quot` sizeOf dummy :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize :: forall m. PrimMonad m => MVector alloc (PrimState m) a -> m ()
  basicInitialize = coerce (G.basicInitialize @VSM.MVector @a @m)

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead :: forall m. PrimMonad m => MVector alloc (PrimState m) a -> Int -> m a
  basicUnsafeRead = coerce (G.basicUnsafeRead @VSM.MVector @a @m)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite :: forall m. PrimMonad m => MVector alloc (PrimState m) a -> Int -> a -> m ()
  basicUnsafeWrite = coerce (G.basicUnsafeWrite @VSM.MVector @a @m)

  {-# INLINE basicSet #-}
  basicSet :: forall m. PrimMonad m => MVector alloc (PrimState m) a -> a -> m ()
  basicSet = coerce (G.basicSet @VSM.MVector @a @m)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy :: forall m. PrimMonad m => MVector alloc (PrimState m) a -> MVector alloc (PrimState m) a -> m ()
  basicUnsafeCopy = coerce (G.basicUnsafeCopy @VSM.MVector @a @m)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove :: forall m. PrimMonad m => MVector alloc (PrimState m) a -> MVector alloc (PrimState m) a -> m ()
  basicUnsafeMove = coerce (G.basicUnsafeMove @VSM.MVector @a @m)

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: (Storable a, Allocator alloc) => MVector alloc s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty
null :: (Storable a, Allocator alloc) => MVector alloc s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: (Storable a, Allocator alloc) => Int -> Int -> MVector alloc s a -> MVector alloc s a
{-# INLINE slice #-}
slice = G.slice

take :: (Storable a, Allocator alloc) => Int -> MVector alloc s a -> MVector alloc s a
{-# INLINE take #-}
take = G.take

drop :: (Storable a, Allocator alloc) => Int -> MVector alloc s a -> MVector alloc s a
{-# INLINE drop #-}
drop = G.drop

splitAt :: (Storable a, Allocator alloc) => Int -> MVector alloc s a -> (MVector alloc s a, MVector alloc s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

init :: (Storable a, Allocator alloc) => MVector alloc s a -> MVector alloc s a
{-# INLINE init #-}
init = G.init

tail :: (Storable a, Allocator alloc) => MVector alloc s a -> MVector alloc s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: (Storable a, Allocator alloc)
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector alloc s a
            -> MVector alloc s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeTake :: (Storable a, Allocator alloc) => Int -> MVector alloc s a -> MVector alloc s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: (Storable a, Allocator alloc) => Int -> MVector alloc s a -> MVector alloc s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

unsafeInit :: (Storable a, Allocator alloc) => MVector alloc s a -> MVector alloc s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: (Storable a, Allocator alloc) => MVector alloc s a -> MVector alloc s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: (Storable a, Allocator alloc) => MVector alloc s a -> MVector alloc s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Storable a, Allocator alloc) => Int -> m (MVector alloc (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The memory is not initialized.
unsafeNew :: (PrimMonad m, Storable a, Allocator alloc) => Int -> m (MVector alloc (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, Storable a, Allocator alloc) => Int -> a -> m (MVector alloc (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, Storable a, Allocator alloc) => Int -> m a -> m (MVector alloc (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Storable a, Allocator alloc)
      => MVector alloc (PrimState m) a -> m (MVector alloc (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, Storable a, Allocator alloc)
     => MVector alloc (PrimState m) a -> Int -> m (MVector alloc (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, Storable a, Allocator alloc)
           => MVector alloc (PrimState m) a -> Int -> m (MVector alloc (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write
    :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Swap the elements at the given positions.
swap
    :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite
    :: (PrimMonad m, Storable a, Allocator alloc) =>  MVector alloc (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap
    :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Storable a, Allocator alloc) => MVector alloc (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Storable a, Allocator alloc)
     => MVector alloc (PrimState m) a   -- ^ target
     -> MVector alloc (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Storable a, Allocator alloc)
           => MVector alloc (PrimState m) a   -- ^ target
           -> MVector alloc (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, Storable a, Allocator alloc)
     => MVector alloc (PrimState m) a -> MVector alloc (PrimState m) a -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, Storable a, Allocator alloc)
           => MVector alloc (PrimState m) a   -- ^ target
           -> MVector alloc (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- Unsafe conversions
-- ------------------

-- | /O(1)/ Unsafely cast a mutable vector from one element type to another.
-- The operation just changes the type of the underlying pointer and does not
-- modify the elements.
--
-- The resulting vector contains as many elements as can fit into the
-- underlying memory block.
--
unsafeCast :: forall a b s alloc.
              (Storable a, Storable b) => MVector alloc s a -> MVector alloc s b
{-# INLINE unsafeCast #-}
unsafeCast = coerce (VSM.unsafeCast @a @b @s)

-- Raw pointers
-- ------------

-- | Create a mutable vector from a 'ForeignPtr' with an offset and a length.
--
-- Modifying data through the 'ForeignPtr' afterwards is unsafe if the vector
-- could have been frozen before the modification.
--
--  If your offset is 0 it is more efficient to use 'unsafeFromForeignPtr0'.
unsafeFromForeignPtr :: forall a s alloc. Storable a
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> MVector alloc s a
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr = coerce (VSM.unsafeFromForeignPtr @a @s)


-- | /O(1)/ Create a mutable vector from a 'ForeignPtr' and a length.
--
-- It is assumed the pointer points directly to the data (no offset).
-- Use `unsafeFromForeignPtr` if you need to specify an offset.
--
-- Modifying data through the 'ForeignPtr' afterwards is unsafe if the vector
-- could have been frozen before the modification.
unsafeFromForeignPtr0 :: forall a s alloc. Storable a
                      => ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> MVector alloc s a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 = coerce (VSM.unsafeFromForeignPtr0 @a @s)

-- | Yield the underlying 'ForeignPtr' together with the offset to the data
-- and its length. Modifying the data through the 'ForeignPtr' is
-- unsafe if the vector could have frozen before the modification.
unsafeToForeignPtr :: forall a s alloc. (Storable a, Allocator alloc) => MVector alloc s a -> (ForeignPtr a, Int, Int)
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr = coerce (VSM.unsafeToForeignPtr @a @s)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with its length.
--
-- You can assume the pointer points directly to the data (no offset).
--
-- Modifying the data through the 'ForeignPtr' is unsafe if the vector could
-- have frozen before the modification.
unsafeToForeignPtr0 :: forall a s alloc. (Storable a, Allocator alloc) => MVector alloc s a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 = coerce (VSM.unsafeToForeignPtr0 @a @s)

-- | Pass a pointer to the vector's data to the IO action. Modifying data
-- through the pointer is unsafe if the vector could have been frozen before
-- the modification.
unsafeWith :: forall a b alloc. (Storable a, Allocator alloc) => IOVector alloc a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith = coerce (VSM.unsafeWith @a @b)
