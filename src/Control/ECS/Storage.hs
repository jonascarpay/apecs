{-# LANGUAGE StandaloneDeriving, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Control.ECS.Storage where

import Control.Monad

type ID = Int

class SStorage IO (Storage c) => Component c where
  type Storage c :: *

class Monad m => SStorage m s where
  type SElem s :: *
  type SSafeElem s :: *

  sEmpty    :: m s
  sSlice    :: s -> m [Entity a]
  sMember   :: s -> Entity a -> m Bool
  sDestroy  :: s -> Entity a -> m ()
  sRetrieve :: s -> Entity a -> m (SSafeElem s)
  sStore    :: s -> SSafeElem s -> Entity a -> m ()
  sOver     :: s -> (SElem s -> SElem s) -> m ()
  sForC     :: s -> (SElem s -> m a) -> m ()

newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Num, Show)

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( Monad m, SStorage m sa, SStorage m sb) => SStorage m (sa, sb) where
  type SSafeElem (sa, sb) = (SSafeElem sa, SSafeElem sb)
  type SElem     (sa, sb) = (SSafeElem sa, SSafeElem sb)

  sEmpty = liftM2 (,) sEmpty sEmpty
  sSlice    (sa,sb) = sSlice sa >>= filterM (sMember sb)
  sMember   (sa,sb) ety = liftM2 (&&) (sMember sa ety) (sMember sb ety)
  sDestroy  (sa,sb) ety = sDestroy sa ety >> sDestroy sb ety
  sRetrieve (sa,sb) ety = liftM2 (,) (sRetrieve sa ety) (sRetrieve sb ety)
  sStore    (sa,sb) (xa,xb) ety = sStore sa xa ety >> sStore sb xb ety

  sOver s f = do sl <- sSlice s
                 forM_ sl $ \ety ->
                   do r  <- sRetrieve s ety
                      sStore s (f r) ety

  sForC s f = do sl <- sSlice s
                 forM_ sl $ \ety ->
                   do r <- sRetrieve s ety
                      f r

  {-# INLINE sEmpty #-}
  {-# INLINE sStore #-}
  {-# INLINE sOver #-}
  {-# INLINE sForC #-}
  {-# INLINE sSlice #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRetrieve #-}
