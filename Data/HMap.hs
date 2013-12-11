{-# LANGUAGE  RankNTypes, GADTs, CPP, EmptyDataDecls #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HMap
-- Copyright   :  (c) Atze van der Ploeg 2013
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of heterogeneous maps.
-- 
-- A heterogeneous map can store values of different types. This in contrast
-- to a homogenous map (such as the one in 'Data.Map') which can store
-- values of a single type.
--
--  For example, here we use
-- a map with 'String' (name), 'Double' (salary) and 'Bool' (female):
--
-- > import Data.HMap 
-- > 
-- > -- type can be inferred.
-- > example ::  Key x String -> Key x1 Double -> Key x2 Bool 
-- >            -> String
-- > example name salary female = 
-- >   format a ++ "\n" ++ format b ++ "\n"
-- >   where a = insert name "Edsger" $ 
-- >             insert salary 4450.0 $ 
-- >             insert female False empty
-- >         b = insert name "Ada"    $ 
-- >             insert salary 5000.0 $ 
-- >             insert female True empty
-- >         format x = x ! name ++ 
-- >                    ": salary=" ++ show (x ! salary) ++ 
-- >                    ", female="  ++ show (x ! female)
-- >
-- > keyLocal :: String
-- > keyLocal = withKey $ withKey $ withKey example
-- >
-- > keyGlobal :: IO String
-- > keyGlobal = 
-- >   do name   <- createKey
-- >      salary <- createKey
-- >      female <- createKey
-- >      return $ example name salary female
-- >                     
-- > main = do print "local"
-- >           putStr keyLocal
-- >           print "global"
-- >           keyGlobal >>= putStr
-- 
-- Which gives:
--
-- > "local"
-- > Edsger: salary=4450.0, female=False
-- > Ada: salary=5000.0, female=True
-- > "global"
-- > Edsger: salary=4450.0, female=False
-- > Ada: salary=5000.0, female=True
-- 
-- Key types carry two type arguments: the scope of the key and
--  the the type of things that can be stored at this key, for example @String@ or @Int@.
--
-- The scope of the key depends on how it is created: 
--
-- * In the @keyLocal@ example, keys are created /locally/ with the 'withKey' function.
--   The type of the 'withKey' function is @(forall x. Key x a -> b) -> b@, which means it 
--   assigns a key and passes it to the given function. The key cannot 
--   escape the function (this would yield a type error). Hence,
--   we say the key is /scoped/ to the function. The scope type argument of the key is then an existential type.
-- 
-- * In the @keyGlobal@ example, keys are created /globally/ with 'createKey' in the IO monad.
--   This allows to create keys that are not 
--   not scoped to a single function, but to the whole program. The scope type argument of the key is then
--   'T'.
--                       
-- This module differs from hackage package @hetero-map@ in the following ways:
--
-- * Lookup, insertion and updates are /O(log n)/ when using this module,
--   whereas they are /O(n)/ when using @hetero-map@.
-- 
-- * With this module we cannot statically ensure that a Heterogenous map 
--   has a some key (i.e. (!) might throw error, like in 'Data.Map').
--   With @hetero-map@ it is possible to statically rule out 
--   such errors.
--
-- * The interface of this module is more similar to  'Data.Map'.
--
-- This module differs from @stable-maps@ in the following ways:
-- 
-- * Key can be created safely without using the IO monad.
--
-- * The interface is more uniform and implements more of the
--    'Data.Map' interface.
-- 
-- * This module uses a Hashmap as a backend, whereas @stable-maps@ uses @Data.Map@.
--   Hashmaps are faster, see <http://blog.johantibell.com/2012/03/announcing-unordered-containers-02.html>.
--
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.HMap (HMap)
-- >  import qualified Data.HMap as HMap
--
-- This module uses @Data.HashMap.Lazy@ as a backend. Every function from 'Data.Map'
-- that makes sense in a heterogenous setting has been implemented.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
--
-----------------------------------------------------------------------------



module Data.HMap(
             -- * HMap type
             HMap  

            -- * Keys
            , Key
            , withKey
            , T
            , createKey
            -- * Operators
            , (!), (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , lookup
            , findWithDefault

            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith

            -- ** Delete\/Update
            , delete
            , adjust
            , update
            , alter

            -- * Combine

            -- ** Union
            , union
            , unions

            -- ** Difference
            , difference

            -- ** Intersection
            , intersection
            )
where
import Prelude hiding (lookup,null)
import Unsafe.Coerce
import Data.Unique
import System.IO.Unsafe
import Data.Hashable
import Data.HashMap.Lazy(HashMap)

import qualified Data.HashMap.Lazy as M
import Data.Maybe(fromJust)



{--------------------------------------------------------------------
  HMap
--------------------------------------------------------------------}


instance Hashable Unique where
  hashWithSalt n u = n + hashUnique u

-- | The type of hetrogenous maps. 
newtype HMap = HMap (HashMap Unique HideType) 

{--------------------------------------------------------------------
  Keys
--------------------------------------------------------------------}

-- | The datatype of Keys. 
--
--   [x] The scope of this key. This can either be 'T' for top-level keys created with 'createKey' or 
--       an existential type for keys introduced by 'withKey'.
-- 
--   [a] The type of things that can be sorted at this key.
-- 
--  For example, @Key T Int@ is a top-level key that can be used to store values
--  of type @Int@ in a heterogenous map.     
newtype Key s a = Key Unique

data HideType where
  HideType :: a -> HideType



unsafeFromHideType :: HideType -> a
unsafeFromHideType (HideType x) = unsafeCoerce x

-- | /O(1)/. Scopes a key to the given function
-- The key cannot escape the function (because of the existential type).
--
-- The implementation actually *creates* a key, but because the key cannot escape
-- the given function @f@, there is no way to observe that if we run 
-- @withKey f@ twice, that it will get a different key the second time.

withKey :: (forall x. Key x a -> b) -> b
withKey f = f $ Key $ unsafePerformIO newUnique
{-# NOINLINE withKey #-} 

-- | The scope of top-level keys.
data T 

-- | /O(1)/. Create a new top-level key.
createKey :: IO (Key T a)
createKey = fmap Key newUnique

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.

(!) :: HMap -> Key x a -> a
m ! k = find k m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE (!) #-}
#endif

-- | Same as 'difference'.
(\\) :: HMap -> HMap -> HMap
m1 \\ m2 = difference m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE (\\) #-}
#endif
{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?


null :: HMap -> Bool
null (HMap m) = M.null m
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the map.


size :: HMap -> Int
size (HMap m) = M.size m
{-# INLINE size #-}


-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.

lookup :: Key x a -> HMap -> Maybe a
lookup (Key x) (HMap m) = fmap unsafeFromHideType (M.lookup x m)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#else
{-# INLINE lookup #-}
#endif

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member ::  Key x a -> HMap -> Bool
member (Key x) (HMap m) = M.member x m
 
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE member #-}
#else
{-# INLINE member #-}
#endif

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.

notMember :: Key x a -> HMap -> Bool
notMember k m = not $ member k m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE notMember #-}
#else
{-# INLINE notMember #-}
#endif

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
find :: Key x a -> HMap -> a
find x m = fromJust $ lookup x m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE find #-}
#else
{-# INLINE find #-}
#endif

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.

findWithDefault :: a -> Key x a -> HMap -> a
findWithDefault a k m = case lookup k m of
                   Just x -> x
                   Nothing -> a 

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.

empty :: HMap
empty = HMap M.empty
{-# INLINE empty #-}

-- | /O(1)/. A map with a single element.


singleton :: Key x a -> a -> HMap
singleton (Key k) x = HMap (M.singleton k (HideType x))
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.

insert :: Key x a -> a -> HMap -> HMap
insert (Key k) a (HMap m) = HMap (M.insert k (HideType a) m)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#else
{-# INLINE insert #-}
#endif


-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.

insertWith :: (a -> a -> a) -> Key x a -> a -> HMap -> HMap 
insertWith f k a m = insert k a' m  
  where a' = case lookup k m of
              Just x  -> f a x
              Nothing ->  a
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#else
{-# INLINE insertWith #-}
#endif

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.

delete :: Key x a -> HMap -> HMap
delete (Key k) (HMap m) = HMap $ M.delete k m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#else
{-# INLINE delete #-}
#endif

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.

adjust :: (a -> a) -> Key x a -> HMap -> HMap
adjust f k m = case lookup k m of
       Just x  -> insert k (f x) m
       Nothing -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#else
{-# INLINE adjust #-}
#endif


-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.

update :: (a -> Maybe a) -> Key x a -> HMap -> HMap
update f k m = case lookup k m of
       Just x  -> case f x of
                   Just y -> insert k y m
                   Nothing -> delete k m
       Nothing -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE update #-}
#else
{-# INLINE update #-}
#endif

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.

alter :: (Maybe a -> Maybe a) -> Key x a -> HMap -> HMap
alter f k m = case f (lookup k m) of
       Just x  -> insert k x m
       Nothing -> delete k m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE alter #-}
#else
{-# INLINE alter #-}
#endif

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).

unions :: [HMap] -> HMap
unions ts
  = foldl union empty ts
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unions #-}
#endif

-- | /O(n+m)/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered.

union :: HMap -> HMap -> HMap
union (HMap l) (HMap r) = HMap (M.union l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE union #-}
#endif


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
difference :: HMap -> HMap -> HMap
difference (HMap l) (HMap r) = HMap (M.difference l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE difference #-}
#endif

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.

intersection :: HMap -> HMap -> HMap
intersection (HMap l) (HMap r) = HMap (M.intersection l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersection #-}
#endif


