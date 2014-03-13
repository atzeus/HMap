{-# LANGUAGE  ScopedTypeVariables,RankNTypes, GADTs, CPP, EmptyDataDecls #-} 
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
            , runKeyT) where

import Unsafe.Coerce
import Data.Unique
import System.IO.Unsafe
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
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

data GD s m a where
  Lift :: m a -> GD s m a
  GetKey :: GD s m (HKey s a)

data TermM f a where
  Return :: a -> TermM f a
  Bind   :: TermM f a -> (a -> TermM f b) -> TermM f b
  Prim   :: f a -> TermM f a

instance Monad (TermM f) where
  return = Return
  (>>=)  = Bind

type Bind f a v = (forall w. f w -> (w -> TermM f a) -> v)

interpret :: Bind f a v -> (a -> v) -> TermM f a ->  v
interpret bind ret = int where
  int (Return a) = ret a
  int (Bind (Prim x) f) = bind x f
  int (Bind (Return x) f) = int (f x)
  int (Bind (Bind p q) r) = int (Bind p (\x -> Bind (q x) r))


-- | A monad that can be used to create keys
--   Keys cannot escape the monad, analogous to the ST Monad.
--   Can be used instead of the 'withKey' function if you
--   need an statically unknown number of keys.
type KeyM s a = KeyT s Identity a
newtype KeyT s m a = KeyT { getKT :: TermM (GD s m) a }

instance Monad (KeyT s m) where
  return   = KeyT . Return
  c >>= f  = KeyT $ getKT c >>= getKT . f


-- | Obtain a key in the key monad
getKey :: KeyT s m (HKey s a)
getKey = KeyT $ Bind (Prim GetKey) Return
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE getKey #-}
#endif

instance MonadTrans (KeyT s) where
  lift m = KeyT (Prim (Lift m))




-- | Run a key monad. Existential type makes sure keys cannot escape.
runKeyT :: forall m a. Monad m => (forall s. KeyT s m a) -> m a
runKeyT (KeyT m) = loop m where
  loop = interpret bind return  where
  bind :: Bind (GD T m) a (m a)
  bind (Lift m) c = m >>= loop . c
  bind GetKey  c = loop (c $ unsafePerformIO $ createKey)




