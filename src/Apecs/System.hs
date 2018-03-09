{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}

module Apecs.System where

import           Control.Monad.Reader
import qualified Data.Vector.Unboxed  as U

import           Apecs.Types

-- | Run a system with a game world
{-# INLINE runSystem #-}
runSystem :: System w a -> w -> IO a
runSystem sys = runReaderT (unSystem sys)

-- | Run a system with a game world
{-# INLINE runWith #-}
runWith :: w -> System w a -> IO a
runWith = flip runSystem

{-# INLINE get #-}
get :: forall w c. Has w (Maybe c) => Entity -> System w (Maybe c)
get (Entity ety) = do
  s :: Storage (Maybe c) <- getStore
  liftIO$ explGet s ety

{-# INLINE get' #-}
get' :: forall w c. Has w c => Entity -> System w c
get' (Entity ety) = do
  s :: Storage c <- getStore
  liftIO$ explGet s ety

-- | Writes a component to a given entity. Will overwrite existing components.
--   The type was originally 'Entity c -> c -> System w ()', but is relaxed to 'Entity e'
--   so you don't always have to write 'set . cast'
{-# INLINE set #-}
set :: forall w c. Has w c => Entity -> c -> System w ()
set (Entity ety) x = do
  s :: Storage c <- getStore
  liftIO$ explSet s ety x

-- | Returns whether the given entity has component @c@
--   For composite components, this indicates whether the component
--   has all its constituents
--   Note that @c@ is a phantom argument, used only to convey the type of the entity to be destroyed.
{-# INLINE exists #-}
exists :: forall w c. Has w c => Entity -> c -> System w Bool
exists (Entity ety) _ = do
  s :: Storage c <- getStore
  liftIO$ explExists s ety

-- | Maps a function over all entities with a @cx@, and writes their @cy@
{-# INLINE cmap #-}
cmap :: forall world cx cy. (Has world cx, Has world cy)
     => (cx -> cy) -> System world ()
cmap f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  liftIO$ do
    sl <- liftIO$ explMembers sx
    U.forM_ sl $ \ e -> do
      r <- explGet sx e
      explSet sy e (f r)

-- | Monadically iterates over all entites with a cx
{-# INLINE cmapM #-}
cmapM :: forall world c a. Has world c
      => (c -> System world a) -> System world [a]
cmapM sys = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  forM (U.toList sl) $ \ ety -> do
    x <- liftIO$ explGet s ety
    sys x

-- | Monadically iterates over all entites with a cx
{-# INLINE cmapM_ #-}
cmapM_ :: forall world c. Has world c
       => (c -> System world ()) -> System world ()
cmapM_ sys = do
  s :: Storage c <- getStore
  sl <- liftIO$ explMembers s
  U.forM_ sl $ \ ety -> do
    x <- liftIO$ explGet s ety
    sys x

-- | Destroys component @c@ for the given entity.
-- Note that @c@ is a phantom argument, used only to convey the type of the entity to be destroyed.
{-# INLINE destroy #-}
destroy :: forall w c. Has w c => Entity -> c -> System w ()
destroy (Entity ety) _ = do
  s :: Storage c <- getStore
  liftIO$ explDestroy s ety

-- | Removes all components. Equivalent to manually iterating and deleting, but usually optimized.
--   Note that @c@ is a phantom argument, used only to convey the type of the entity to be destroyed.
{-# INLINE resetStore #-}
resetStore :: forall w c p. Has w c => p c -> System w ()
resetStore _ = do s :: Storage c <- getStore
                  liftIO$ explReset s

-- | Applies a function, if possible.
{-# INLINE modify #-}
modify :: forall w c. Has w c => Entity -> (c -> c) -> System w ()
modify (Entity ety) f = do
  s :: Storage c <- getStore
  liftIO$ do
    x <- explGet s ety
    explSet s ety (f x)
