{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}

module Apecs.System where

import           Control.Monad.Reader
import           Data.Proxy
import qualified Data.Vector.Unboxed  as U

import           Apecs.Core

-- | Run a system with a game world
{-# INLINE runSystem #-}
runSystem :: System w a -> w -> IO a
runSystem sys = runReaderT (unSystem sys)

-- | Run a system with a game world
{-# INLINE runWith #-}
runWith :: w -> System w a -> IO a
runWith = flip runSystem

{-# INLINE get #-}
get :: forall w c. Get w c => Entity -> System w c
get (Entity ety) = do
  s :: Storage c <- getStore
  liftIO$ explGet s ety

-- | Writes a component to a given entity. Will overwrite existing components.
--   The type was originally 'Entity c -> c -> System w ()', but is relaxed to 'Entity e'
--   so you don't always have to write 'set . cast'
{-# INLINE set #-}
set :: forall w c. Set w c => Entity -> c -> System w ()
set (Entity ety) x = do
  s :: Storage c <- getStore
  liftIO$ explSet s ety x

-- | Returns whether the given entity has component @c@
--   Note that @c@ is a phantom argument, used only to convey the type of the entity to be queried.
{-# INLINE exists #-}
exists :: forall w c. Get w c => Entity -> Proxy c -> System w Bool
exists (Entity ety) _ = do
  s :: Storage c <- getStore
  liftIO$ explExists s ety

-- | Maps a function over all entities with a @cx@, and writes their @cy@.
{-# INLINE cmap #-}
cmap :: forall w cx cy. (Get w cx, Members w cx, Set w cy)
     => (cx -> cy) -> System w ()
cmap f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  liftIO$ do
    sl <- explMembers sx
    U.forM_ sl $ \ e -> do
      r <- explGet sx e
      explSet sy e (f r)

-- | Monadically iterates over all entites with a @cx@, and writes their @cy@.
{-# INLINE cmapM #-}
cmapM :: forall w cx cy. (Get w cx, Set w cy, Members w cx)
      => (cx -> System w cy) -> System w ()
cmapM sys = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  sl <- liftIO$ explMembers sx
  U.forM_ sl $ \ e -> do
    x <- liftIO$ explGet sx e
    y <- sys x
    liftIO$ explSet sy e y

-- | Monadically iterates over all entites with a @cx@
{-# INLINE cmapM_ #-}
cmapM_ :: forall w c a. (Get w c, Members w c)
       => (c -> System w a) -> System w ()
cmapM_ sys = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  U.forM_ sl $ \ ety -> do
    x <- liftIO$ explGet s ety
    sys x

-- | Fold over the game world; for example, @cfold max (minBound :: Foo)@ will find the maximum value of @Foo@.
--   Strict in the accumulator.
{-# INLINE cfold #-}
cfold :: forall w c a. (Members w c, Get w c)
      => (a -> c -> a) -> a -> System w a
cfold f a0 = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  liftIO$ U.foldM' (\a e -> f a <$> explGet s e) a0 sl

-- | Monadically fold over the game world.
--   Strict in the accumulator.
{-# INLINE cfoldM #-}
cfoldM :: forall w c a. (Members w c, Get w c)
       => (a -> c -> System w a) -> a -> System w a
cfoldM sys a0 = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  U.foldM' (\a e -> liftIO (explGet s e) >>= sys a) a0 sl

-- | Monadically fold over the game world.
--   Strict in the accumulator.
{-# INLINE cfoldM_ #-}
cfoldM_ :: forall w c a. (Members w c, Get w c)
       => (a -> c -> System w a) -> a -> System w ()
cfoldM_ sys a0 = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  U.foldM'_ (\a e -> liftIO (explGet s e) >>= sys a) a0 sl

-- | Get all components @c@.
--   Call as @[(c,Entity)]@ to also read the entity index.
{-# INLINE getAll #-}
getAll :: forall w c. (Get w c, Members w c)
      => System w [c]
getAll = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  forM (U.toList sl) $ liftIO . explGet s

-- | Destroys component @c@ for the given entity.
-- Note that @c@ is a phantom argument, used only to convey the type of the entity to be destroyed.
{-# INLINE destroy #-}
destroy :: forall w c. Destroy w c => Entity -> Proxy c -> System w ()
destroy (Entity ety) ~_ = do
  s :: Storage c <- getStore
  liftIO$ explDestroy s ety

-- | Applies a function, if possible.
{-# INLINE modify #-}
modify :: forall w c. (Get w c, Set w c) => Entity -> (c -> c) -> System w ()
modify (Entity ety) f = do
  s :: Storage c <- getStore
  liftIO$ do
    x <- explGet s ety
    explSet s ety (f x)

-- | Counts the number of entities with a @c@
{-# INLINE count #-}
count :: forall w c. Members w c => Proxy c -> System w Int
count ~_ = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  return $ U.length sl
