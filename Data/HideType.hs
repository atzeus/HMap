{-# LANGUAGE   GADTs #-} 
module Data.HideType where

import Unsafe.Coerce

data HideType where
  HideType :: a -> HideType

unsafeFromHideType :: HideType -> a
unsafeFromHideType (HideType x) = unsafeCoerce x
