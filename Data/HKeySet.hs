{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HKeySet
-- Copyright   :  (c) Atze van der Ploeg 2013
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A set of 'HKey's
module Data.HKeySet

 (
      HKeySet
    -- * Construction
    , empty
    , singleton

    -- * Combine
    , union
    , unions

    -- * Basic interface
    , null
    , size
    , member
    , insert
    , delete


    -- * Difference and intersection
    , difference
    , intersection
    -- * KeySet-HMap functions
    , overlap
    , sameKeys
    , removeKeys
    )
 where



import           Data.HKey
import           Data.HMap   (HMap)
import qualified Data.HMap   as S
import qualified Data.List   as List
import           Data.Unique
import           Prelude     hiding (null)
-- | The type of hetrogenous key sets.
newtype HKeySet = HKeySet HMap


-- | /O(1)/ Construct an empty key set.
empty :: HKeySet
empty = HKeySet S.empty


-- | /O(1)/ Construct a set with a single element.
singleton :: HKey s a -> HKeySet
singleton x = HKeySet (S.singleton x undefined)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE singleton #-}
#endif

-- | /O(n+m)/ Construct a key set containing all elements from both sets.
--
-- To obtain good performance, the smaller set must be presented as
-- the first argument.
union :: HKeySet -> HKeySet -> HKeySet
union (HKeySet s1) (HKeySet s2) = HKeySet (s1 `S.union` s2)
{-# INLINE union #-}

-- | Construct a key set containing all elements from a list of key sets.
#if __GLASGOW_HASKELL__ >= 710
unions :: Foldable f => f HKeySet -> HKeySet
#endif
unions = List.foldl' union empty
{-# INLINE unions #-}

-- | /O(1)/ Return 'True' if this  key set is empty, 'False' otherwise.
null :: HKeySet -> Bool
null (HKeySet x) = S.null x
{-# INLINE null #-}

-- | /O(n)/ Return the number of elements in this set.
size :: HKeySet -> Int
size (HKeySet x) = S.size x
{-# INLINE size #-}

-- | /O(min(n,W))/ Return 'True' if the given value is present in this
-- set, 'False' otherwise.
member :: HKey s a -> HKeySet -> Bool
member x (HKeySet s) = x `S.member` s
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE member #-}
#endif

-- | /O(min(n,W))/ Add the specified value to this set.
insert :: HKey x a -> HKeySet  -> HKeySet
insert x (HKeySet s) = HKeySet (S.insert x undefined s)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(min(n,W))/ Remove the specified value from this set if
-- present.
delete :: HKey s a -> HKeySet -> HKeySet
delete x (HKeySet s) = HKeySet (S.delete x s)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

-- | /O(n)/ Difference of two sets. Return elements of the first set
-- not existing in the second.
difference :: HKeySet -> HKeySet -> HKeySet
difference (HKeySet a) (HKeySet b) = HKeySet (S.difference a b)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE difference #-}
#endif

-- | /O(n)/ Intersection of two sets. Return elements present in both
-- the first set and the second.
intersection :: HKeySet -> HKeySet -> HKeySet
intersection (HKeySet a) (HKeySet b) = HKeySet (S.intersection a b)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersection #-}
#endif

{--------------------------------------------------------------------
  KeySet-HMap functions
--------------------------------------------------------------------}
-- | /O(n)/. Does the map carry any of the keys?
overlap :: HMap -> HKeySet -> Bool
overlap h (HKeySet s) = not $ S.null (h `S.intersection` s)

-- | /O(n)/. Do the keyset and the map have the same keys?
sameKeys :: HMap -> HKeySet -> Bool
sameKeys h (HKeySet s) =  S.null (h `S.difference` s)

-- | /O(n)/. Remove the keys from the keyset from the map.
removeKeys :: HMap -> HKeySet -> HMap
removeKeys h (HKeySet s) = h `S.difference` s
