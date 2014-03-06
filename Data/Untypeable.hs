-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HMap
-- Copyright   :  (c) Atze van der Ploeg 2013
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a Typeable-like interface for things that cannot derive typeable.

module Data.Untypeable(
   Untypeable,
   inject,
   project)
   where

import Data.HKeyPrivate
import Data.HideType
import Data.Unique

data Untypeable = Untypeable Unique HideType

-- | Make an Untypeable value
inject :: HKey s a -> a -> Untypeable
inject (Key x) a = Untypeable x (HideType a)

-- | Project (i.e. cast) an untypeable value with a given key.
project :: HKey s a -> Untypeable -> Maybe a
project (Key x) (Untypeable x' h) 
  | x == x'   = Just $ unsafeFromHideType h
  | otherwise = Nothing


