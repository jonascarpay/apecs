{-# LANGUAGE UndecidableInstances, BangPatterns, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, ConstraintKinds #-}

{-# LANGUAGE Strict #-}

module Control.ECS.Core where

import Control.Monad.Reader
import qualified Data.Vector.Unboxed as U

type ID    = Int
type IDVec = U.Vector ID

{-
  TODO: Can this be expressed in streaming libraries in a more elegant way?
  TODO: Is it useful to further split this into more compositional elements?
-}
-- | A SStorage instance is a mutable component container.
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

-- | A component is defined by the type of its storage
--   The storage in turn supplies runtime types for the component.
class SStorage (Storage c) => Component c where
  type Storage c :: *

newtype System w a = System {unSystem :: ReaderT w IO a} deriving (Functor, Monad, Applicative, MonadIO)

-- | These following hj
newtype Store  c = Store  {unStore  :: Storage c}
newtype Slice  c = Slice  {unSlice  :: U.Vector ID} deriving Show
newtype Reads  c = Reads  {unReads  :: SSafeElem (Storage c)}
newtype Writes c = Writes {unWrites :: SSafeElem (Storage c)}
newtype Elem   c = Elem   {unElem   :: SElem     (Storage c)}
newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Num)

instance Eq (SSafeElem (Storage c)) => Eq (Reads c) where
  Reads a == Reads b = a == b

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

{-# INLINE sliceAll #-}
sliceAll :: forall w c. Has w c => System w (Slice c)
sliceAll = do Store s :: Store c <- getStore
              fmap Slice . liftIO $ sAll s

{-# INLINE exists #-}
exists :: forall w c. Has w c => Entity c -> System w Bool
exists (Entity ety) = do Store s :: Store c <- getStore
                         liftIO $ sMember s ety

{-# INLINE read #-}
read :: forall w c. Has w c => Entity c -> System w (Reads c)
read (Entity ety) = do Store s :: Store c <- getStore
                       fmap Reads . liftIO $ sRead s ety

{-# INLINE write #-}
write :: forall w c. Has w c => Writes c -> Entity c -> System w ()
write (Writes w) (Entity ety) = do Store s :: Store c <- getStore
                                   liftIO $ sWrite s w ety

{-# INLINE writeRaw #-}
writeRaw :: forall w c. Has w c => Elem c -> Entity c -> System w ()
writeRaw (Elem w) (Entity ety) =
  do Store s :: Store c <- getStore
     liftIO $ sWriteUnsafe s w ety


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
                      do !r <- sReadUnsafe s e
                         let Elem w = f (Elem r)
                         sWriteUnsafe s w e

instance (Has wld r, Has wld w) => PureFunction (Elem r -> Writes w) wld where
  {-# INLINE apply #-}
  apply f = do Store !sr :: Store r <- getStore
               Store !sw :: Store w <- getStore
               liftIO $
                 do !vec <- sAll sr
                    U.forM_ vec $ \e ->
                      do !r <- sReadUnsafe sr e
                         let Writes w = f (Elem r)
                         sWrite sw w e

instance (Has wld r, Has wld w) => PureFunction (Reads r -> Elem w) wld where
  {-# INLINE apply #-}
  apply f = do Store !sr :: Store r <- getStore
               Store !sw :: Store w <- getStore
               liftIO $
                 do !vec <- sAll sw
                    U.forM_ vec $ \e ->
                      do !r <- sRead sr e
                         let Elem w = f (Reads r)
                         sWriteUnsafe sw w e

instance (Has wld r, Has wld w) => PureFunction (Entity a, Reads r -> Writes w) wld where
  {-# INLINE apply #-}
  apply (Entity e, f) =
    do Store !sr :: Store r <- getStore
       Store !sw :: Store w <- getStore
       liftIO $
         do !r <- sRead sr e
            let Writes w = f (Reads r)
            sWrite sw w e

instance (Has wld r, Has wld w) => PureFunction (Slice a, Reads r -> Writes w) wld where
  {-# INLINE apply #-}
  apply (Slice vec, f) =
    do Store !sr :: Store r <- getStore
       Store !sw :: Store w <- getStore
       liftIO $ U.forM_ vec $ \e ->
              do !r <- sRead sr e
                 let Writes w = f (Reads r)
                 sWrite sw w e

{-# INLINE sliceForM_ #-}
sliceForM_ :: forall w s e r a. Has w r => Slice s -> ((Entity e, Reads r) -> System w a) -> System w ()
sliceForM_ (Slice vec) fm =
  do Store s :: Store r <- getStore
     U.forM_ vec (\e -> do !r <- liftIO$ sRead s e
                           fm (Entity e, Reads r))

sliceMapM_ :: forall w s e r a. Has w r => ((Entity e, Reads r) -> System w a) -> Slice s -> System w ()
sliceMapM_ = flip Control.ECS.Core.sliceForM_

sliceFoldM_ :: (a -> Entity c -> System w a) -> a -> Slice b -> System w ()
sliceFoldM_ f seed (Slice sl) = U.foldM'_ ((.Entity) . f) seed sl

-- | Gets the size of a slice (O(n))
sliceSize :: Slice a -> Int
sliceSize (Slice vec) = U.length vec

-- | Tests whether a slice is empty (O(1))
sliceNull :: Slice a -> Bool
sliceNull (Slice vec) = U.null vec

-- | Construct a slice from a list of IDs
sliceFromList :: [ID] -> Slice a
sliceFromList = Slice . U.fromList

-- | Monadically filter a slice
sliceFilterM :: (Entity c -> System w Bool) -> Slice c -> System w (Slice c)
sliceFilterM fm (Slice vec) = Slice <$> U.filterM (fm . Entity) vec

sliceConcat :: Slice a -> Slice b -> Slice c
sliceConcat (Slice a) (Slice b) = Slice (a U.++ b)

class Cast a b where
  cast :: a -> b

instance Cast (Entity a) (Entity b) where cast (Entity e) = Entity e
instance Cast (Slice a) (Slice b) where cast (Slice e) = Slice e
