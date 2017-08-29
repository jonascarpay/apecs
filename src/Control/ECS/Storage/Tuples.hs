{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.ECS.Storage.Tuples where

import qualified Data.Vector.Unboxed as U
import Control.Monad

import Control.ECS.Core

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( Monad m, SStorage m sa, SStorage m sb) => SStorage m (sa, sb) where
  type SSafeElem (sa, sb) = (SSafeElem sa, SSafeElem sb)
  type SElem     (sa, sb) = (SElem     sa, SElem     sb)

  sEmpty = liftM2 (,) sEmpty sEmpty
  sSlice    (!sa,!sb) = sSlice sa >>= U.filterM (sMember sb)
  sMember   (!sa,!sb) ety = liftM2 (&&) (sMember sa ety) (sMember sb ety)
  sDestroy  (!sa,!sb) ety = sDestroy sa ety >> sDestroy sb ety
  sRetrieve (!sa,!sb) ety = liftM2 (,) (sRetrieve sa ety) (sRetrieve sb ety)
  sStore    (!sa,!sb) (wa,wb) ety = sStore sa wa ety >> sStore sb wb ety

  sWUnsafe  (!sa,!sb) (wa,wb) ety = sWUnsafe sa wa ety >> sWUnsafe sb wb ety
  sRUnsafe  (!sa,!sb) ety = liftM2 (,) (sRUnsafe sa ety) (sRUnsafe sb ety)

  {-# INLINE sEmpty #-}
  {-# INLINE sStore #-}
  {-# INLINE sWUnsafe #-}
  {-# INLINE sRUnsafe #-}
  {-# INLINE sSlice #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRetrieve #-}

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  {-# INLINE getStore #-}
  getStore = do Store sa :: Store a <- getStore
                Store sb :: Store b <- getStore
                return $ Store (sa, sb)
