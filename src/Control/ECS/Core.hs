{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, ConstraintKinds #-}

{-# LANGUAGE Strict #-}

module Control.ECS.Core where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Vector.Unboxed as U

type ID = Int
type IDVec = U.Vector ID

class SStorage s where
  type SElem s :: *
  type SSafeElem s :: *

  sEmpty    :: IO s
  sSlice    :: s -> IO IDVec
  sMember   :: s -> ID -> IO Bool
  sDestroy  :: s -> ID -> IO ()
  sRetrieve :: s -> ID -> IO (SSafeElem s)
  sStore    :: s -> SSafeElem s -> ID -> IO ()

  sWUnsafe  :: s -> SElem s -> ID -> IO ()
  sRUnsafe  :: s -> ID -> IO (SElem s)

class SStorage (Storage c) => Component c where
  type Storage c :: *

newtype Store  c = Store  {unStore  :: Storage c}
class w `Has` c where
  getStore :: System w (Store c)

type Valid w c = (Has w c, Component c, SStorage (Storage c))

newtype System w a = System {unSystem :: ReaderT w IO a} deriving (Functor, Monad, Applicative, MonadIO)

newtype Reads  c = Reads  {unReads  :: SSafeElem (Storage c)}
newtype Writes c = Writes {unWrites :: SSafeElem (Storage c)}
newtype Elem   c = Elem   {unElem   :: SElem (Storage c)}

newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Num)
instance Show (Entity c) where
  show (Entity e) = "Entity " ++ show e

newtype Slice  c = Slice {unSlice :: U.Vector ID}

{-# INLINE runSystem #-}
runSystem :: System w a -> w -> IO a
runSystem sys = runReaderT (unSystem sys)

{-# INLINE runWith #-}
runWith :: w -> System w a -> IO a
runWith = flip runSystem

{-# INLINE empty #-}
empty :: SStorage (Storage c) => IO (Store c)
empty = Store <$> sEmpty

{-# INLINE slice #-}
slice :: forall w c. Valid w c => System w (Slice c)
slice = do Store s :: Store c <- getStore
           fmap Slice . liftIO $ sSlice s

{-# INLINE isMember #-}
isMember :: forall w c. Valid w c => Entity c -> System w Bool
isMember (Entity ety) = do Store s :: Store c <- getStore
                           liftIO $ sMember s ety

{-# INLINE retrieve #-}
retrieve :: forall w c a. Valid w c => Entity a -> System w (Reads c)
retrieve (Entity ety) = do Store s :: Store c <- getStore
                           fmap Reads . liftIO $ sRetrieve s ety

{-# INLINE store #-}
store :: forall w c a. Valid w c => Writes c -> Entity a -> System w ()
store (Writes w) (Entity ety) = do Store s :: Store c <- getStore
                                   liftIO $ sStore s w ety

{-# INLINE destroy #-}
destroy :: forall w c. Valid w c => Entity c -> System w ()
destroy (Entity ety) = do Store s :: Store c <- getStore
                          liftIO $ sDestroy s ety

{-# INLINE mapRW #-}
mapRW :: forall w c. Valid w c => (Elem c -> Elem c) -> System w ()
mapRW f = do Store s :: Store c <- getStore
             liftIO $ sSlice s >>= U.mapM_ (\e -> do r <- sRUnsafe s e; sWUnsafe s (unElem . f $ Elem r) e)

{-# INLINE mapR #-}
mapR :: forall w r wr. (Valid w r, Valid w wr) => (Elem r -> Writes wr) -> System w ()
mapR !f = do Store !sr :: Store r  <- getStore
             Store !sw :: Store wr <- getStore
             liftIO $ do !sl <- sSlice sr
                         U.forM_ sl $ \ !e -> (do !r <- sRUnsafe sr e; sStore sw (unWrites . f . Elem $ r) e)

{-# INLINE mapM_ #-}
mapM_ :: forall w c a b. Valid w c => ((Entity a, Elem c) -> System w b) -> System w ()
mapM_ fm = do Store s  :: Store c <- getStore
              Slice sl :: Slice c <- slice
              U.forM_ sl (\e -> do r <- liftIO$ sRUnsafe s e
                                   fm (Entity e, Elem r))
