{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- 'Storable'-based vectors with custom allocators.

module Data.Vector.Alloc (
  -- * Storable vectors
  Vector, MVector(..), Storable, Allocator,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic indexing
  indexM, headM, lastM,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, iterateNM, create, createT,

  -- ** Unfolding
  unfoldr, unfoldrN,
  unfoldrM, unfoldrNM,
  constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update_,
  unsafeUpd, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate_,
  unsafeAccum, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, mapM_, forM, forM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,

  -- ** Monadic zipping
  zipWithM, zipWithM_,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, uniq,
  mapMaybe, imapMaybe,
  filterM,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, foldM', fold1M, fold1M',
  foldM_, foldM'_, fold1M_, fold1M'_,

  -- * Prefix sums (scans)
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Other vector types
  G.convert, unsafeCast,

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy,

  -- * Raw pointers
  unsafeFromForeignPtr, unsafeFromForeignPtr0,
  unsafeToForeignPtr,   unsafeToForeignPtr0,
  unsafeWith
) where

import qualified Data.Vector.Generic          as G
import           Data.Vector.Alloc.Mutable ( MVector(..), Allocator )
import qualified Data.Vector.Storable as VS

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr

import Control.DeepSeq ( NFData )

import Control.Monad.ST ( ST )
import Control.Monad.Primitive

import qualified GHC.Exts as Exts

import Data.Coerce

import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_ )

import Data.Typeable  ( Typeable )
import Data.Data      ( Data(..) )
import Data.Semigroup ( Semigroup(..) )

-- | 'Storable'-based vectors
newtype Vector alloc a = Vector (VS.Vector a)
  deriving ( Typeable, NFData, Show, Read, Eq, Ord, Semigroup, Monoid )

instance (Typeable alloc, Data a, Storable a, Allocator alloc) => Data (Vector alloc a) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = G.mkType "Data.Vector.Alloc.Vector"
  dataCast1    = G.dataCast

type instance G.Mutable (Vector alloc) = MVector alloc

instance (Storable a, Allocator alloc) => G.Vector (Vector alloc) a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector vs) = Vector <$> G.basicUnsafeFreeze vs

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector vs) = MVector <$> G.basicUnsafeThaw vs

  {-# INLINE basicLength #-}
  basicLength  = coerce (G.basicLength @VS.Vector @a)

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = coerce (G.basicUnsafeSlice @VS.Vector @a)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM :: forall m. Monad m => Vector alloc a -> Int -> m a
  basicUnsafeIndexM = coerce (G.basicUnsafeIndexM @VS.Vector @a @m)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy :: forall m. PrimMonad m => G.Mutable (Vector alloc) (PrimState m) a -> Vector alloc a -> m ()
  basicUnsafeCopy = coerce (G.basicUnsafeCopy @VS.Vector @a @m)

  {-# INLINE elemseq #-}
  elemseq _ = seq

instance (Storable a, Allocator alloc) => Exts.IsList (Vector alloc a) where
  type Item (Vector alloc a) = a
  fromList = fromList
  fromListN = fromListN
  toList = toList

-- Length
-- ------

-- | /O(1)/ Yield the length of the vector
length :: (Storable a, Allocator alloc) => Vector alloc a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector is empty
null :: (Storable a, Allocator alloc) => Vector alloc a -> Bool
{-# INLINE null #-}
null = G.null

-- Indexing
-- --------

-- | O(1) Indexing
(!) :: (Storable a, Allocator alloc) => Vector alloc a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing
(!?) :: (Storable a, Allocator alloc) => Vector alloc a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element
head :: (Storable a, Allocator alloc) => Vector alloc a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element
last :: (Storable a, Allocator alloc) => Vector alloc a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: (Storable a, Allocator alloc) => Vector alloc a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: (Storable a, Allocator alloc) => Vector alloc a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: (Storable a, Allocator alloc) => Vector alloc a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- Monadic indexing
-- ----------------

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the vector when necessary.
-- Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
-- For lazy vectors, @v ! i@ would not be evaluated which means that @mv@
-- would unnecessarily retain a reference to @v@ in each element written.
--
-- With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >                   x <- indexM v i
-- >                   write mv i x
--
-- Here, no references to @v@ are retained because indexing (but /not/ the
-- elements) is evaluated eagerly.
--
indexM :: (Storable a, Allocator alloc, Monad m) => Vector alloc a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (Storable a, Allocator alloc, Monad m) => Vector alloc a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Storable a, Allocator alloc, Monad m) => Vector alloc a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Storable a, Allocator alloc, Monad m) => Vector alloc a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Storable a, Allocator alloc, Monad m) => Vector alloc a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Storable a, Allocator alloc, Monad m) => Vector alloc a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: (Storable a, Allocator alloc)
      => Int   -- ^ @i@ starting index
      -> Int   -- ^ @n@ length
      -> Vector alloc a
      -> Vector alloc a
{-# INLINE slice #-}
slice = G.slice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE init #-}
init = G.init

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE tail #-}
tail = G.tail

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: (Storable a, Allocator alloc) => Int -> Vector alloc a -> Vector alloc a
{-# INLINE take #-}
take = G.take

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: (Storable a, Allocator alloc) => Int -> Vector alloc a -> Vector alloc a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-# INLINE splitAt #-}
splitAt :: (Storable a, Allocator alloc) => Int -> Vector alloc a -> (Vector alloc a, Vector alloc a)
splitAt = G.splitAt

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: (Storable a, Allocator alloc) => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector alloc a
                       -> Vector alloc a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: (Storable a, Allocator alloc) => Int -> Vector alloc a -> Vector alloc a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: (Storable a, Allocator alloc) => Int -> Vector alloc a -> Vector alloc a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: (Storable a, Allocator alloc) => Vector alloc a
{-# INLINE empty #-}
empty = G.empty

-- | /O(1)/ Vector alloc with exactly one element
singleton :: (Storable a, Allocator alloc) => a -> Vector alloc a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ Vector alloc of the given length with the same value in each position
replicate :: (Storable a, Allocator alloc) => Int -> a -> Vector alloc a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: (Storable a, Allocator alloc) => Int -> (Int -> a) -> Vector alloc a
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: (Storable a, Allocator alloc) => Int -> (a -> a) -> a -> Vector alloc a
{-# INLINE iterateN #-}
iterateN = G.iterateN

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: (Storable a, Allocator alloc) => (b -> Maybe (a, b)) -> b -> Vector alloc a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: (Storable a, Allocator alloc) => Int -> (b -> Maybe (a, b)) -> b -> Vector alloc a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrM :: (Monad m, Storable a, Allocator alloc) => (b -> m (Maybe (a, b))) -> b -> m (Vector alloc a)
{-# INLINE unfoldrM #-}
unfoldrM = G.unfoldrM

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrNM :: (Monad m, Storable a, Allocator alloc) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector alloc a)
{-# INLINE unfoldrNM #-}
unfoldrNM = G.unfoldrNM

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
constructN :: (Storable a, Allocator alloc) => Int -> (Vector alloc a -> a) -> Vector alloc a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
constructrN :: (Storable a, Allocator alloc) => Int -> (Vector alloc a -> a) -> Vector alloc a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Storable a, Num a, Allocator alloc) => a -> Int -> Vector alloc a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: (Storable a, Num a, Allocator alloc) => a -> a -> Int -> Vector alloc a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Storable a, Enum a, Allocator alloc) => a -> a -> Vector alloc a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Storable a, Enum a, Allocator alloc) => a -> a -> a -> Vector alloc a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: (Storable a, Allocator alloc) => a -> Vector alloc a -> Vector alloc a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element
snoc :: (Storable a, Allocator alloc) => Vector alloc a -> a -> Vector alloc a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a -> Vector alloc a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list
concat :: (Storable a, Allocator alloc) => [Vector alloc a] -> Vector alloc a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, Storable a, Allocator alloc) => Int -> m a -> m (Vector alloc a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, Storable a, Allocator alloc) => Int -> (Int -> m a) -> m (Vector alloc a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | /O(n)/ Apply monadic function n times to value. Zeroth element is original value.
iterateNM :: (Monad m, Storable a, Allocator alloc) => Int -> (a -> m a) -> a -> m (Vector alloc a)
{-# INLINE iterateNM #-}
iterateNM = G.iterateNM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: (Storable a, Allocator alloc) => (forall s. ST s (MVector alloc s a)) -> Vector alloc a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = G.create p

-- | Execute the monadic action and freeze the resulting vectors.
createT :: (Traversable f, Storable a, Allocator alloc) => (forall s. ST s (f (MVector alloc s a))) -> f (Vector alloc a)
{-# INLINE createT #-}
createT p = G.createT p

-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
force :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: (Storable a, Allocator alloc) => Vector alloc a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector alloc a
{-# INLINE (//) #-}
(//) = (G.//)

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
update_ :: (Storable a, Allocator alloc)
        => Vector alloc a   -- ^ initial vector (of length @m@)
        -> Vector alloc Int -- ^ index vector (of length @n1@)
        -> Vector alloc a   -- ^ value vector (of length @n2@)
        -> Vector alloc a
{-# INLINE update_ #-}
update_ = G.update_

-- | Same as ('//') but without bounds checking.
unsafeUpd :: (Storable a, Allocator alloc) => Vector alloc a -> [(Int, a)] -> Vector alloc a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc Int -> Vector alloc a -> Vector alloc a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: (Storable a, Allocator alloc)
      => (a -> b -> a) -- ^ accumulating function @f@
      -> Vector alloc a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector alloc a
{-# INLINE accum #-}
accum = G.accum

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
accumulate_ :: (Storable a, Storable b, Allocator alloc)
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector alloc a      -- ^ initial vector (of length @m@)
            -> Vector alloc Int    -- ^ index vector (of length @n1@)
            -> Vector alloc b      -- ^ value vector (of length @n2@)
            -> Vector alloc a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: (Storable a, Allocator alloc) => (a -> b -> a) -> Vector alloc a -> [(Int,b)] -> Vector alloc a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (Storable a, Storable b, Allocator alloc) =>
               (a -> b -> a) -> Vector alloc a -> Vector alloc Int -> Vector alloc b -> Vector alloc a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE reverse #-}
reverse = G.reverse

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc Int -> Vector alloc a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: (Storable a, Allocator alloc) => Vector alloc a -> Vector alloc Int -> Vector alloc a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation will be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise.
--
-- @
-- modify (\\v -> write v 0 \'x\') ('replicate' 3 \'a\') = \<\'x\',\'a\',\'a\'\>
-- @
modify :: (Storable a, Allocator alloc) => (forall s. MVector alloc s a -> ST s ()) -> Vector alloc a -> Vector alloc a
{-# INLINE modify #-}
modify p = G.modify p

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (Storable a, Storable b, Allocator alloc) => (a -> b) -> Vector alloc a -> Vector alloc b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Storable a, Storable b, Allocator alloc) => (Int -> a -> b) -> Vector alloc a -> Vector alloc b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a vector and concatenate the results.
concatMap :: (Storable a, Storable b, Allocator alloc) => (a -> Vector alloc b) -> Vector alloc a -> Vector alloc b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, Storable a, Storable b, Allocator alloc) => (a -> m b) -> Vector alloc a -> m (Vector alloc b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: (Monad m, Storable a, Allocator alloc) => (a -> m b) -> Vector alloc a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equivalent to @flip 'mapM'@.
forM :: (Monad m, Storable a, Storable b, Allocator alloc) => Vector alloc a -> (a -> m b) -> m (Vector alloc b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, Storable a, Allocator alloc) => Vector alloc a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (Storable a, Storable b, Storable c, Allocator alloc)
        => (a -> b -> c) -> Vector alloc a -> Vector alloc b -> Vector alloc c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Storable a, Storable b, Storable c, Storable d, Allocator alloc)
         => (a -> b -> c -> d) -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e, Allocator alloc)
         => (a -> b -> c -> d -> e)
         -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d -> Vector alloc e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f, Allocator alloc)
         => (a -> b -> c -> d -> e -> f)
         -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d -> Vector alloc e
         -> Vector alloc f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f, Storable g, Allocator alloc)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d -> Vector alloc e
         -> Vector alloc f -> Vector alloc g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Storable a, Storable b, Storable c, Allocator alloc)
         => (Int -> a -> b -> c) -> Vector alloc a -> Vector alloc b -> Vector alloc c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Storable a, Storable b, Storable c, Storable d, Allocator alloc)
          => (Int -> a -> b -> c -> d)
          -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e, Allocator alloc)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d -> Vector alloc e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
              Storable f, Allocator alloc)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d -> Vector alloc e
          -> Vector alloc f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
              Storable f, Storable g, Allocator alloc)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector alloc a -> Vector alloc b -> Vector alloc c -> Vector alloc d -> Vector alloc e
          -> Vector alloc f -> Vector alloc g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, Storable a, Storable b, Storable c, Allocator alloc)
         => (a -> b -> m c) -> Vector alloc a -> Vector alloc b -> m (Vector alloc c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, Storable a, Storable b, Allocator alloc)
          => (a -> b -> m c) -> Vector alloc a -> Vector alloc b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Vector alloc a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: (Storable a, Allocator alloc) => (Int -> a -> Bool) -> Vector alloc a -> Vector alloc a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop repeated adjacent elements.
uniq :: (Storable a, Eq a, Allocator alloc) => Vector alloc a -> Vector alloc a
{-# INLINE uniq #-}
uniq = G.uniq

-- | /O(n)/ Drop elements when predicate returns Nothing
mapMaybe :: (Storable a, Storable b, Allocator alloc) => (a -> Maybe b) -> Vector alloc a -> Vector alloc b
{-# INLINE mapMaybe #-}
mapMaybe = G.mapMaybe

-- | /O(n)/ Drop elements when predicate, applied to index and value, returns Nothing
imapMaybe :: (Storable a, Storable b, Allocator alloc) => (Int -> a -> Maybe b) -> Vector alloc a -> Vector alloc b
{-# INLINE imapMaybe #-}
imapMaybe = G.imapMaybe

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, Storable a, Allocator alloc) => (a -> m Bool) -> Vector alloc a -> m (Vector alloc a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
takeWhile :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Vector alloc a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Vector alloc a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> (Vector alloc a, Vector alloc a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> (Vector alloc a, Vector alloc a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
span :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> (Vector alloc a, Vector alloc a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
break :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> (Vector alloc a, Vector alloc a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (Storable a, Eq a, Allocator alloc) => a -> Vector alloc a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (Storable a, Eq a, Allocator alloc) => a -> Vector alloc a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Vector alloc Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Storable a, Eq a, Allocator alloc) => a -> Vector alloc a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (Storable a, Eq a, Allocator alloc) => a -> Vector alloc a -> Vector alloc Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: (Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator
foldl' :: (Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold
foldr :: (Storable a, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: (Storable a, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: (Storable b, Allocator alloc) => (a -> Int -> b -> a) -> a -> Vector alloc b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: (Storable b, Allocator alloc) => (a -> Int -> b -> a) -> a -> Vector alloc b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: (Storable a, Allocator alloc) => (Int -> a -> b -> b) -> b -> Vector alloc a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: (Storable a, Allocator alloc) => (Int -> a -> b -> b) -> b -> Vector alloc a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
any :: (Storable a, Allocator alloc) => (a -> Bool) -> Vector alloc a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'
and :: Allocator alloc => Vector alloc Bool -> Bool
{-# INLINE and #-}
and = G.and

-- | /O(n)/ Check if any element is 'True'
or :: Allocator alloc => Vector alloc Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements
sum :: (Storable a, Num a, Allocator alloc) => Vector alloc a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the produce of the elements
product :: (Storable a, Num a, Allocator alloc) => Vector alloc a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (Storable a, Ord a, Allocator alloc) => Vector alloc a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the vector according to the given
-- comparison function. The vector may not be empty.
maximumBy :: (Storable a, Allocator alloc) => (a -> a -> Ordering) -> Vector alloc a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (Storable a, Ord a, Allocator alloc) => Vector alloc a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
minimumBy :: (Storable a, Allocator alloc) => (a -> a -> Ordering) -> Vector alloc a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (Storable a, Ord a, Allocator alloc) => Vector alloc a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the vector according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: (Storable a, Allocator alloc) => (a -> a -> Ordering) -> Vector alloc a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (Storable a, Ord a, Allocator alloc) => Vector alloc a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: (Storable a, Allocator alloc) => (a -> a -> Ordering) -> Vector alloc a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, Storable b, Allocator alloc) => (a -> b -> m a) -> a -> Vector alloc b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, Storable a, Allocator alloc) => (a -> a -> m a) -> Vector alloc a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, Storable b, Allocator alloc) => (a -> b -> m a) -> a -> Vector alloc b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Storable a, Allocator alloc) => (a -> a -> m a) -> Vector alloc a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, Storable b, Allocator alloc) => (a -> b -> m a) -> a -> Vector alloc b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, Storable a, Allocator alloc) => (a -> a -> m a) -> Vector alloc a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, Storable b, Allocator alloc) => (a -> b -> m a) -> a -> Vector alloc b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, Storable a, Allocator alloc) => (a -> a -> m a) -> Vector alloc a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

-- Prefix sums (scans)
-- -------------------

-- | /O(n)/ Prescan
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
--
prescanl :: (Storable a, Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> Vector alloc a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (Storable a, Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> Vector alloc a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: (Storable a, Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> Vector alloc a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (Storable a, Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> Vector alloc a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--
scanl :: (Storable a, Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> Vector alloc a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (Storable a, Storable b, Allocator alloc) => (a -> b -> a) -> a -> Vector alloc b -> Vector alloc a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> Vector alloc a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> Vector alloc a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (Storable a, Storable b, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> Vector alloc b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (Storable a, Storable b, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> Vector alloc b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left scan
postscanr :: (Storable a, Storable b, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> Vector alloc b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (Storable a, Storable b, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> Vector alloc b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (Storable a, Storable b, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> Vector alloc b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (Storable a, Storable b, Allocator alloc) => (a -> b -> b) -> b -> Vector alloc a -> Vector alloc b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> Vector alloc a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: (Storable a, Allocator alloc) => (a -> a -> a) -> Vector alloc a -> Vector alloc a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: (Storable a, Allocator alloc) => Vector alloc a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector
fromList :: (Storable a, Allocator alloc) => [a] -> Vector alloc a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: (Storable a, Allocator alloc) => Int -> [a] -> Vector alloc a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Unsafe casts
-- --------------------------

-- | /O(1)/ Unsafely cast a vector from one element type to another.
-- The operation just changes the type of the underlying pointer and does not
-- modify the elements.
--
-- The resulting vector contains as many elements as can fit into the
-- underlying memory block.
--
unsafeCast :: forall a b alloc. (Storable a, Storable b, Allocator alloc) => Vector alloc a -> Vector alloc b
{-# INLINE unsafeCast #-}
unsafeCast = coerce (VS.unsafeCast @a @b)


-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze
        :: (Storable a, PrimMonad m, Allocator alloc) => MVector alloc (PrimState m) a -> m (Vector alloc a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one without
-- copying. The immutable vector may not be used after this operation.
unsafeThaw
        :: (Storable a, PrimMonad m, Allocator alloc) => Vector alloc a -> m (MVector alloc (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (Storable a, PrimMonad m, Allocator alloc) => Vector alloc a -> m (MVector alloc (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (Storable a, PrimMonad m, Allocator alloc) => MVector alloc (PrimState m) a -> m (Vector alloc a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (Storable a, PrimMonad m, Allocator alloc) => MVector alloc (PrimState m) a -> Vector alloc a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (Storable a, PrimMonad m, Allocator alloc) => MVector alloc (PrimState m) a -> Vector alloc a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- Conversions - Raw pointers
-- --------------------------

-- | /O(1)/ Create a vector from a 'ForeignPtr' with an offset and a length.
--
-- The data may not be modified through the 'ForeignPtr' afterwards.
--
-- If your offset is 0 it is more efficient to use 'unsafeFromForeignPtr0'.
unsafeFromForeignPtr :: forall a alloc. (Storable a, Allocator alloc)
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> Vector alloc a
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr = coerce (VS.unsafeFromForeignPtr @a)


-- | /O(1)/ Create a vector from a 'ForeignPtr' and a length.
--
-- It is assumed the pointer points directly to the data (no offset).
-- Use `unsafeFromForeignPtr` if you need to specify an offset.
--
-- The data may not be modified through the 'ForeignPtr' afterwards.
unsafeFromForeignPtr0 :: forall a alloc. (Storable a, Allocator alloc)
                      => ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> Vector alloc a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 = coerce (VS.unsafeFromForeignPtr0 @a)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with the offset to the
-- data and its length. The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr :: forall a alloc. (Storable a, Allocator alloc) => Vector alloc a -> (ForeignPtr a, Int, Int)
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr = coerce (VS.unsafeToForeignPtr @a)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with its length.
--
-- You can assume the pointer points directly to the data (no offset).
--
-- The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr0 :: forall a alloc. (Storable a, Allocator alloc) => Vector alloc a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 = coerce (VS.unsafeToForeignPtr0 @a)

-- | Pass a pointer to the vector's data to the IO action. The data may not be
-- modified through the 'Ptr.
unsafeWith :: forall a b alloc. (Storable a, Allocator alloc) => Vector alloc a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith = coerce (VS.unsafeWith @a @b)