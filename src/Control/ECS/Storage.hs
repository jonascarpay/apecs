{-# LANGUAGE StandaloneDeriving, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Control.ECS.Storage where

import Control.Monad
import qualified Data.Vector.Unboxed as U

type ID = Int
type IDVec = U.Vector ID

class SStorage IO (Storage c) => Component c where
  type Storage c :: *

class Monad m => SStorage m s where
  type SElem s :: *
  type SSafeElem s :: *

  sEmpty    :: m s
  sSlice    :: s -> m IDVec
  sMember   :: s -> ID -> m Bool
  sDestroy  :: s -> ID -> m ()
  sRetrieve :: s -> ID -> m (SSafeElem s)
  sStore    :: s -> SSafeElem s -> ID -> m ()

  sWUnsafe  :: s -> SElem s -> ID -> m ()
  sRUnsafe  :: s -> ID -> m (SElem s)

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
