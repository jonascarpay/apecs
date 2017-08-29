{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.ECS.Storage.Tuples where

import qualified Data.Vector.Unboxed as U
import Control.Monad

import Control.ECS.Core

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance (SStorage sa, SStorage sb) => SStorage (sa, sb) where
  type SSafeElem (sa, sb) = (SSafeElem sa, SSafeElem sb)
  type SElem     (sa, sb) = (SElem     sa, SElem     sb)

  sEmpty = liftM2 (,) sEmpty sEmpty
  sAll    (!sa,!sb) = sAll sa >>= U.filterM (sMember sb)
  sMember   (!sa,!sb) ety = liftM2 (&&) (sMember sa ety) (sMember sb ety)
  sDestroy  (!sa,!sb) ety = sDestroy sa ety >> sDestroy sb ety
  sRead (!sa,!sb) ety = liftM2 (,) (sRead sa ety) (sRead sb ety)
  sWrite    (!sa,!sb) (wa,wb) ety = sWrite sa wa ety >> sWrite sb wb ety

  sWriteUnsafe  (!sa,!sb) (wa,wb) ety = sWriteUnsafe sa wa ety >> sWriteUnsafe sb wb ety
  sReadUnsafe  (!sa,!sb) ety = liftM2 (,) (sReadUnsafe sa ety) (sReadUnsafe sb ety)

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  {-# INLINE getStore #-}
  getStore = do Store sa :: Store a <- getStore
                Store sb :: Store b <- getStore
                return $ Store (sa, sb)
