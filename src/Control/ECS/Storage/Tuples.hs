{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.ECS.Storage.Tuples where

import qualified Data.Vector.Unboxed as U
import Control.Monad

import Control.ECS.Core

-- (,)
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

-- (,,)
instance (Component a, Component b, Component c) => Component (a, b, c) where
  type Storage (a, b, c) = (Storage a, Storage b, Storage c)

instance (SStorage sa, SStorage sb, SStorage sc) => SStorage (sa, sb, sc) where
  type SSafeElem (sa, sb, sc) = (SSafeElem sa, SSafeElem sb, SSafeElem sc)
  type SElem     (sa, sb, sc) = (SElem     sa, SElem     sb, SElem     sc)

  sEmpty = liftM3 (,,) sEmpty sEmpty sEmpty
  sAll      (!sa,!sb,!sc) = sAll sa >>= U.filterM (sMember sb) >>= U.filterM (sMember sc)
  sMember   (!sa,!sb,!sc) ety = do ma <- sMember sa ety; mb <- sMember sb ety; mc <- sMember sc ety; return (ma && mb && mc)
  sDestroy  (!sa,!sb,!sc) ety = sDestroy sa ety >> sDestroy sb ety >> sDestroy sc ety
  sRead     (!sa,!sb,!sc) ety = liftM3 (,,) (sRead sa ety) (sRead sb ety) (sRead sc ety)
  sWrite    (!sa,!sb,!sc) (wa,wb,wc) ety = sWrite sa wa ety >> sWrite sb wb ety >> sWrite sc wc ety

  sWriteUnsafe  (!sa,!sb,!sc) (wa,wb,wc) ety = sWriteUnsafe sa wa ety >> sWriteUnsafe sb wb ety >> sWriteUnsafe sc wc ety
  sReadUnsafe   (!sa,!sb,!sc) ety = liftM3 (,,) (sReadUnsafe sa ety) (sReadUnsafe sb ety) (sReadUnsafe sc ety)

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}

instance (w `Has` a, w `Has` b, w `Has` c) => w `Has` (a, b, c) where
  {-# INLINE getStore #-}
  getStore = do Store sa :: Store a <- getStore
                Store sb :: Store b <- getStore
                Store sc :: Store c <- getStore
                return $ Store (sa, sb, sc)

-- (,,,)
instance (Component a, Component b, Component c, Component d) => Component (a, b, c, d) where
  type Storage (a, b, c, d) = (Storage a, Storage b, Storage c, Storage d)

instance (SStorage sa, SStorage sb, SStorage sc, SStorage sd) => SStorage (sa, sb, sc, sd) where
  type SSafeElem (sa, sb, sc, sd) = (SSafeElem sa, SSafeElem sb, SSafeElem sc, SSafeElem sd)
  type SElem     (sa, sb, sc, sd) = (SElem     sa, SElem     sb, SElem     sc, SElem     sd)

  sEmpty = liftM4 (,,,) sEmpty sEmpty sEmpty sEmpty
  sAll      (!sa,!sb,!sc,!sd) = sAll sa >>= U.filterM (sMember sb) >>= U.filterM (sMember sc) >>= U.filterM (sMember sd)
  sMember   (!sa,!sb,!sc,!sd) ety = do ma <- sMember sa ety; mb <- sMember sb ety; mc <- sMember sc ety; md <- sMember sd ety; return (ma && mb && mc && md)
  sDestroy  (!sa,!sb,!sc,!sd) ety = sDestroy sa ety >> sDestroy sb ety >> sDestroy sc ety >> sDestroy sd ety
  sRead     (!sa,!sb,!sc,!sd) ety = liftM4 (,,,) (sRead sa ety) (sRead sb ety) (sRead sc ety) (sRead sd ety)
  sWrite    (!sa,!sb,!sc,!sd) (wa,wb,wc,wd) ety = sWrite sa wa ety >> sWrite sb wb ety >> sWrite sc wc ety >> sWrite sd wd ety

  sWriteUnsafe  (!sa,!sb,!sc,!sd) (wa,wb,wc,wd) ety = sWriteUnsafe sa wa ety >> sWriteUnsafe sb wb ety >> sWriteUnsafe sc wc ety >> sWriteUnsafe sd wd ety
  sReadUnsafe   (!sa,!sb,!sc,!sd) ety = liftM4 (,,,) (sReadUnsafe sa ety) (sReadUnsafe sb ety) (sReadUnsafe sc ety) (sReadUnsafe sd ety)

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}

instance (w `Has` a, w `Has` b, w `Has` c, w `Has` d) => w `Has` (a, b, c, d) where
  {-# INLINE getStore #-}
  getStore = do Store sa :: Store a <- getStore
                Store sb :: Store b <- getStore
                Store sc :: Store c <- getStore
                Store sd :: Store d <- getStore
                return $ Store (sa, sb, sc, sd)
