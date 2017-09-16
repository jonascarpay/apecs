{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Apecs.System where

import Control.Monad.Reader

import Apecs.Types

-- | Run a system with a game world
{-# INLINE runSystem #-}
runSystem :: System w a -> w -> IO a
runSystem sys = runReaderT (unSystem sys)

-- | Run a system with a game world
{-# INLINE runWith #-}
runWith :: w -> System w a -> IO a
runWith = flip runSystem

-- | A slice containing all entities with component @c@
{-# INLINE owners #-}
owners :: forall w c. (Has w c, HasMembers (Storage c)) => System w (Slice c)
owners = do s :: Storage c <- getStore
            liftIO$ Slice <$> explMembers s

-- | Returns whether the given entity has component @c@
--   For composite components, this indicates whether the component
--   has all its constituents
{-# INLINE exists #-}
exists :: forall w c. (Has w c, HasMembers (Storage c)) => Entity c -> System w Bool
exists (Entity n) = do s :: Storage c <- getStore
                       liftIO$ explExists s n

-- | Destroys the component @c@ for the given entity
{-# INLINE destroy #-}
destroy :: forall w c. (Has w c, HasMembers (Storage c)) => Entity c -> System w ()
destroy (Entity n) = do s :: Storage c <- getStore
                        liftIO$ explDestroy s n

-- | Removes all components. Equivalent to manually iterating and deleting, but usually optimized.
resetStore :: forall w c p. (Has w c, HasMembers (Storage c)) => p c -> System w ()
resetStore _ = do s :: Storage c <- getStore
                  liftIO$ explReset s

-- Setting/Getting
-- | Gets the component for a given entity.
--   This is a safe access, because the entity might not have the requested components.
{-# INLINE get #-}
get :: forall w c. (Store (Storage c), Has w c) => Entity c -> System w (Safe c)
get (Entity ety) = do s :: Storage c <- getStore
                      liftIO$ Safe <$> explGet s ety

-- | Writes a component to a given entity. Will overwrite existing components.
{-# INLINE set #-}
set :: forall w c e. (Store (Storage c), Stores (Storage c) ~ c, Has w c) => Entity e -> c -> System w ()
set (Entity ety) x = do
  s :: Storage c <- getStore
  liftIO$ explSet s ety x

-- | Same as @set@, but uses Safe to possibly delete a component
setOrDelete :: forall w c. (IsRuntime c, Has w c) => Entity c -> Safe c -> System w ()
setOrDelete (Entity ety) (Safe c) = do
  s :: Storage c <- getStore
  liftIO$ explSetMaybe s ety c

-- | Applies a function if possible. Equivalent to reading, mapping, and writing, but stores can provide optimized implementations.
{-# INLINE modify #-}
modify :: forall w c. (IsRuntime c, Has w c) => Entity c -> (c -> c) -> System w ()
modify (Entity ety) f = do
  s :: Storage c <- getStore
  liftIO$ explModify s ety f

{-# INLINE imapM_ #-}
-- | Monadically iterate a system over all entities that have that component.
--   Note that writing to the store while iterating over it is undefined behaviour.
imapM_ :: forall w c. (Has w c, HasMembers (Storage c))
       => (Entity c -> System w ()) -> System w ()
imapM_ sys = do s :: Storage c <- getStore
                explImapM_ s (sys . Entity)

{-# INLINE imapM #-}
-- | Monadically iterate a system over all entities that have that component.
--   Note that writing to the store while iterating over it is undefined behaviour.
imapM :: forall w c a. (Has w c, HasMembers (Storage c))
      => (Entity c -> System w a) -> System w [a]
imapM sys = do s :: Storage c <- getStore
               explImapM s (sys . Entity)

{-# INLINE cmap #-}
cmap :: forall world c. (IsRuntime c, Has world c) => (c -> c) -> System world ()
cmap f = do s :: Storage c <- getStore
            liftIO$ explCmap s f

{-# INLINE cmapM_ #-}
cmapM_ :: forall w c. (Has w c, IsRuntime c)
       => (c -> System w ()) -> System w ()
cmapM_ sys = do s :: Storage c <- getStore
                explCmapM_ s sys

{-# INLINE cimapM_ #-}
cimapM_ :: forall w c. (Has w c, IsRuntime c)
        => ((Entity c, c) -> System w ()) -> System w ()
cimapM_ sys = do s :: Storage c <- getStore
                 explCimapM_ s (\(e,c) -> sys (Entity e,c))

{-# INLINE cmapM #-}
cmapM :: forall w c a. (Has w c, IsRuntime c)
      => (c -> System w a) -> System w [a]
cmapM sys = do s :: Storage c <- getStore
               explCmapM s sys

{-# INLINE cimapM #-}
cimapM :: forall w c a. (Has w c, IsRuntime c)
       => ((Entity c, c) -> System w a) -> System w [a]
cimapM sys = do s :: Storage c <- getStore
                explCimapM s (\(e,c) -> sys (Entity e,c))

{-# INLINE slice #-}
slice :: forall w c q. (Query q (Storage c), Has w c) => q -> System w (Slice c)
slice q = do
  s :: Storage c <- getStore
  liftIO$ Slice <$> explSlice s q

{-# INLINE readGlobal #-}
readGlobal :: forall w c. (Has w c, GlobalRW (Storage c) c) => System w c
readGlobal = do s :: Storage c <- getStore
                liftIO$ explGlobalRead s

{-# INLINE writeGlobal #-}
writeGlobal :: forall w c. (Has w c, GlobalRW (Storage c) c) => c -> System w ()
writeGlobal c = do s :: Storage c <- getStore
                   liftIO$ explGlobalWrite s c

{-# INLINE modifyGlobal #-}
modifyGlobal :: forall w c. (Has w c, GlobalRW (Storage c) c) => (c -> c) -> System w ()
modifyGlobal f = do s :: Storage c <- getStore
                    liftIO$ explGlobalModify s f

