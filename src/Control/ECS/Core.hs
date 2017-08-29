{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, ConstraintKinds #-}

{-# LANGUAGE Strict #-}

module Control.ECS.Core where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Vector.Unboxed as U

type ID = Int
type IDVec = U.Vector ID

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

class SStorage IO (Storage c) => Component c where
  type Storage c :: *

newtype Store  c = Store  {unStore  :: Storage c}
class w `Has` c where
  getStore :: Monad m => System w m (Store c)

type Valid w m c = (Has w c, Component c, SStorage m (Storage c))

newtype System w m a = System {unSystem :: ReaderT w m a} deriving (Functor, Monad, Applicative, MonadIO, MonadTrans)

newtype Reads  c = Reads  {unReads  :: SSafeElem (Storage c)}
newtype Writes c = Writes {unWrites :: SSafeElem (Storage c)}
newtype Elem   c = Elem   {unElem   :: SElem (Storage c)}

newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Num)
instance Show (Entity c) where
  show (Entity e) = "Entity " ++ show e

newtype Slice  c = Slice {unSlice :: U.Vector ID}

{-# INLINE runSystem #-}
runSystem :: System w m a -> w -> m a
runSystem sys = runReaderT (unSystem sys)

{-# INLINE runWith #-}
runWith :: w -> System w m a -> m a
runWith = flip runSystem

{-# INLINE empty #-}
empty :: SStorage m (Storage c) => m (Store c)
empty = Store <$> sEmpty

{-# INLINE slice #-}
slice :: forall w m c. Valid w m c => System w m (Slice c)
slice = do Store s :: Store c <- getStore
           fmap Slice . lift $ sSlice s

{-# INLINE isMember #-}
isMember :: forall w m c. Valid w m c => Entity c -> System w m Bool
isMember (Entity ety) = do Store s :: Store c <- getStore
                           lift $ sMember s ety

{-# INLINE retrieve #-}
retrieve :: forall w m c a. Valid w m c => Entity a -> System w m (Reads c)
retrieve (Entity ety) = do Store s :: Store c <- getStore
                           fmap Reads . lift $ sRetrieve s ety

{-# INLINE store #-}
store :: forall w m c a. Valid w m c => Writes c -> Entity a -> System w m ()
store (Writes w) (Entity ety) = do Store s :: Store c <- getStore
                                   lift $ sStore s w ety

{-# INLINE destroy #-}
destroy :: forall w m c. Valid w m c => Entity c -> System w m ()
destroy (Entity ety) = do Store s :: Store c <- getStore
                          lift $ sDestroy s ety

{-# INLINE mapRW #-}
mapRW :: forall w m c. Valid w m c => (Elem c -> Elem c) -> System w m ()
mapRW f = do Store s :: Store c <- getStore
             lift $ sSlice s >>= U.mapM_ (\e -> do r <- sRUnsafe s e; sWUnsafe s (unElem . f $ Elem r) e)

{-# INLINE mapR #-}
mapR :: forall w m r wr. (Valid w m r, Valid w m wr) => (Elem r -> Writes wr) -> System w m ()
mapR !f = do Store !sr :: Store r  <- getStore
             Store !sw :: Store wr <- getStore
             lift $ do !sl <- sSlice sr
                       U.forM_ sl $ \ !e -> (do !r <- sRUnsafe sr e; sStore sw (unWrites . f . Elem $ r) e)

{-# INLINE mapM_ #-}
mapM_ :: forall w m c a b. Valid w m c => ((Entity a, Elem c) -> System w m b) -> System w m ()
mapM_ fm = do Store s  :: Store c <- getStore
              Slice sl :: Slice c <- slice
              U.forM_ sl (\e -> do r <- lift$ sRUnsafe s e
                                   fm (Entity e, Elem r))
