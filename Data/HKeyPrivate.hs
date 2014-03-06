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
-- A HKey is a key that can be used in 'HMap','HKeySet' or 'Untypeable'
-- it carries the type of thing it points to in its own type.
module Data.HKeyPrivate( 
              HKey(..)
            , withKey
            , T
            , createKey
            , KeyM
            , getKey
            , runKeyM) where

import Unsafe.Coerce
import Data.Unique
import System.IO.Unsafe
import Control.Monad
import Data.Hashable

instance Hashable Unique where
  hashWithSalt n u = n + hashUnique u


{--------------------------------------------------------------------
  Keys
--------------------------------------------------------------------}

-- | The datatype of Keys. 
--
--   [x] The scope of this key. This can either be 'T' for top-level keys created with 'createKey' or 
--       an existential type for keys introduced by 'withKey' (or with the Key monad 'KeyM').
-- 
--   [a] The type of things that can be sorted at this key.
-- 
--  For example, @Key T Int@ is a top-level key that can be used to store values
--  of type @Int@ in a heterogenous map.     
newtype HKey s a = Key Unique

-- | /O(1)/. Scopes a key to the given function
-- The key cannot escape the function (because of the existential type).
--
-- The implementation actually *creates* a key, but because the key cannot escape
-- the given function @f@, there is no way to observe that if we run 
-- @withKey f@ twice, that it will get a different key the second time.

withKey :: (forall x. HKey x a -> b) -> b
withKey f = unsafePerformIO $ liftM f createKey 
{-# NOINLINE withKey #-} 

-- | The scope of top-level keys.
data T 

-- | /O(1)/. Create a new top-level key.
createKey :: IO (HKey T a)
createKey = fmap Key newUnique


{--------------------------------------------------------------------
  Key Monad
--------------------------------------------------------------------}



data GD s a = Done a
           | forall b. GetKey (HKey s b -> GD s a)

-- uses Codensity monad like definition for speed.
-- | A monad that can be used to create keys
--   Keys cannot escape the monad, analogous to the ST Monad.
--   Can be used instead of the 'withKey' function if you
--   need an statically unkown number of keys.
newtype KeyM s a = KeyM { rk :: forall b. (a -> GD s b) -> GD s b }

instance Monad (KeyM s) where
  return a = KeyM (\k -> k a)
  c >>= f  = KeyM (\k -> rk c (\a -> rk (f a) k))

-- | Obtain a key in the key monad
getKey :: KeyM s (HKey s a)
getKey = KeyM $ \c -> GetKey c
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE getKey #-}
#endif


-- | Run a key monad. Existential type makes sure keys cannot escape.
runKeyM :: (forall s. KeyM s a) -> a
runKeyM m = loop (rk m Done) where
  loop (Done a) = a
  loop (GetKey c) = loop $ unsafePerformIO $ liftM c createKey 
  {-# NOINLINE loop #-} 




