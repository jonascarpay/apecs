{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, ConstraintKinds #-}

{-# LANGUAGE Strict #-}

module Control.ECS.Core where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Vector.Unboxed as U

type ID    = Int
type IDVec = U.Vector ID

class SStorage s where
  type SElem s :: *
  type SSafeElem s :: *

  sEmpty    :: IO s
  sAll      :: s -> IO IDVec
  sMember   :: s -> ID -> IO Bool
  sDestroy  :: s -> ID -> IO ()

  sRead       :: s -> ID -> IO (SSafeElem s)
  sReadUnsafe :: s -> ID -> IO (SElem s)

  sWrite       :: s -> SSafeElem s -> ID -> IO ()
  sWriteUnsafe :: s -> SElem s     -> ID -> IO ()

class SStorage (Storage c) => Component c where
  type Storage c :: *

newtype System w a = System {unSystem :: ReaderT w IO a} deriving (Functor, Monad, Applicative, MonadIO)

newtype Store  c = Store  {unStore  :: Storage c}
newtype Slice  c = Slice  {unSlice  :: U.Vector ID}
newtype Reads  c = Reads  {unReads  :: SSafeElem (Storage c)}
newtype Writes c = Writes {unWrites :: SSafeElem (Storage c)}
newtype Elem   c = Elem   {unElem   :: SElem     (Storage c)}
newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Num)

class Component c => w `Has` c where
  getStore :: System w (Store c)

instance Show (Entity c) where
  show (Entity e) = "Entity " ++ show e

{-# INLINE runSystem #-}
runSystem :: System w a -> w -> IO a
runSystem sys = runReaderT (unSystem sys)

{-# INLINE runWith #-}
runWith :: w -> System w a -> IO a
runWith = flip runSystem

{-# INLINE empty #-}
empty :: SStorage (Storage c) => IO (Store c)
empty = Store <$> sEmpty

{-# INLINE all #-}
all :: forall w c. Has w c => System w (Slice c)
all = do Store s :: Store c <- getStore
         fmap Slice . liftIO $ sAll s

{-# INLINE isMember #-}
isMember :: forall w c. Has w c => Entity c -> System w Bool
isMember (Entity ety) = do Store s :: Store c <- getStore
                           liftIO $ sMember s ety

{-# INLINE read #-}
read :: forall w c a. Has w c => Entity a -> System w (Reads c)
read (Entity ety) = do Store s :: Store c <- getStore
                       fmap Reads . liftIO $ sRead s ety

{-# INLINE write #-}
write :: forall w c a. Has w c => Writes c -> Entity a -> System w ()
write (Writes w) (Entity ety) = do Store s :: Store c <- getStore
                                   liftIO $ sWrite s w ety

{-# INLINE destroy #-}
destroy :: forall w c. Has w c => Entity c -> System w ()
destroy (Entity ety) = do Store s :: Store c <- getStore
                          liftIO $ sDestroy s ety

{--}

class PureFunction f w where
  apply :: f -> System w ()

instance Has w c => PureFunction (Elem c -> Elem c) w where
  {-# INLINE apply #-}
  apply f = do Store !s :: Store c <- getStore
               liftIO $
                 do !vec <- sAll s
                    U.forM_ vec $ \e ->
                      do r <- sReadUnsafe s e
                         let Elem w = f (Elem r)
                         sWriteUnsafe s w e

instance (Has wld r, Has wld w) => PureFunction (Elem r -> Writes w) wld where
  {-# INLINE apply #-}
  apply f = do Store !sr :: Store r <- getStore
               Store !sw :: Store w <- getStore
               liftIO $
                 do !vec <- sAll sr
                    U.forM_ vec $ \e ->
                      do r <- sReadUnsafe sr e
                         let Writes w = f (Elem r)
                         sWrite sw w e

instance (Has wld r, Has wld w) => PureFunction (Reads r -> Elem w) wld where
  {-# INLINE apply #-}
  apply f = do Store !sr :: Store r <- getStore
               Store !sw :: Store w <- getStore
               liftIO $
                 do !vec <- sAll sw
                    U.forM_ vec $ \e ->
                      do r <- sRead sr e
                         let Elem w = f (Reads r)
                         sWriteUnsafe sw w e

instance (Has wld r, Has wld w) => PureFunction (Entity a, Reads r -> Writes w) wld where
  {-# INLINE apply #-}
  apply (Entity e, f) =
    do Store !sr :: Store r <- getStore
       Store !sw :: Store w <- getStore
       liftIO $
         do r <- sRead sr e
            let Writes w = f (Reads r)
            sWrite sw w e

instance (Has wld r, Has wld w) => PureFunction (Slice a, Reads r -> Writes w) wld where
  {-# INLINE apply #-}
  apply (Slice vec, f) =
    do Store !sr :: Store r <- getStore
       Store !sw :: Store w <- getStore
       liftIO $ U.forM_ vec $ \e ->
              do r <- sRead sr e
                 let Writes w = f (Reads r)
                 sWrite sw w e

{-# INLINE forM_ #-}
forM_ :: forall w s e r a. Has w r => Slice s -> ((Entity e, Reads r) -> System w a) -> System w ()
forM_ (Slice vec) fm =
  do Store s :: Store r <- getStore
     U.forM_ vec (\e -> do r <- liftIO$ sRead s e
                           fm (Entity e, Reads r))

mapM_ :: forall w s e r a. Has w r => ((Entity e, Reads r) -> System w a) -> Slice s -> System w ()
mapM_ = flip Control.ECS.Core.forM_

size :: Slice a -> Int
size (Slice vec) = U.length vec
