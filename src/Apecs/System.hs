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

-- | Returns whether the given entity has component @c@
--   For composite components, this indicates whether the component
--   has all its constituents
{-# INLINE exists #-}
exists :: forall w c. Has w c => Entity -> c -> System w Bool
exists (Entity ety) _ = do
  s :: Storage c <- getStore
  liftIO$ explExists s ety

-- | Destroys the component @c@ for the given entity
{-# INLINE destroy #-}
destroy :: forall w c. Has w c => Entity -> c -> System w ()
destroy (Entity ety) _ = do
  s :: Storage c <- getStore
  liftIO$ explDestroy s ety

-- | Removes all components. Equivalent to manually iterating and deleting, but usually optimized.
{-# INLINE resetStore #-}
resetStore :: forall w c p. Has w c => p c -> System w ()
resetStore _ = do s :: Storage c <- getStore
                  liftIO$ explReset s

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

-- | Applies a function, if possible.
{-# INLINE modify #-}
modify :: forall w c. Has w c => Entity -> (c -> c) -> System w ()
modify (Entity ety) f = do
  s :: Storage c <- getStore
  liftIO$ do
    x <- explGet s ety
    explSet s ety (f x)

-- | Maps a function over all entities with a @r@, and writes their @w@
{-# INLINE cmap #-}
cmap :: forall world cx cy. (Has world cx, Has world cy)
     => (cx -> cy) -> System world ()
cmap f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  liftIO$ do
    sl <- explMembers sx
    U.forM_ sl $ \ e -> do
      r <- explGet sx e
      explSet sy e (f r)
