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

-- | A slice containing all entities with component @c@
{-# INLINE owners #-}
owners :: forall w c. Has w c => System w (Slice c)
owners = do s :: Storage c <- getStore
            liftIO$ Slice <$> explMembers s

-- | Returns whether the given entity has component @c@
--   For composite components, this indicates whether the component
--   has all its constituents
{-# INLINE exists #-}
exists :: forall w c. Has w c => Entity c -> System w Bool
exists (Entity n) = do s :: Storage c <- getStore
                       liftIO$ explExists s n

-- | Destroys the component @c@ for the given entity
{-# INLINE destroy #-}
destroy :: forall w c. Has w c => Entity c -> System w ()
destroy (Entity n) = do s :: Storage c <- getStore
                        liftIO$ explDestroy s n

-- | Removes all components. Equivalent to manually iterating and deleting, but usually optimized.
{-# INLINE resetStore #-}
resetStore :: forall w c p. Has w c => p c -> System w ()
resetStore _ = do s :: Storage c <- getStore
                  liftIO$ explReset s

-- Setting/Getting
-- | Gets the component for a given entity.
--   This is a safe access, because the entity might not have the requested components.
{-# INLINE get #-}
get :: forall w c. Has w c => Entity c -> System w (Safe c)
get (Entity ety) = do s :: Storage c <- getStore
                      liftIO$ Safe <$> explGet s ety

-- | Same as @get@, but does not return a safe value and therefore errors if the target component is not present.
{-# INLINE getUnsafe #-}
getUnsafe :: forall w c. Has w c => Entity c -> System w c
getUnsafe (Entity ety) = do s :: Storage c <- getStore
                            liftIO$ explGetUnsafe s ety

-- | Writes a component to a given entity. Will overwrite existing components.
--   The type was originally 'Entity c -> c -> System w ()', but is relaxed to 'Entity e'
--   so you don't always have to write 'set . cast'
{-# INLINE set #-}
set :: forall w c e. Has w c => Entity e -> c -> System w ()
set (Entity ety) x = do
  s :: Storage c <- getStore
  liftIO$ explSet s ety x

-- | Same as @set@, but uses Safe to possibly delete a component
{-# INLINE set' #-}
set' :: forall w c. Has w c => Entity c -> Safe c -> System w ()
set' (Entity ety) (Safe c) = do
  s :: Storage c <- getStore
  liftIO$ explSetMaybe s ety c

-- | Applies a function if possible. Equivalent to reading, mapping, and writing, but stores can provide optimized implementations.
{-# INLINE modify #-}
modify :: forall w c. Has w c => Entity c -> (c -> c) -> System w ()
modify (Entity ety) f = do
  s :: Storage c <- getStore
  liftIO$ explModify s ety f

{-# INLINE imapM_ #-}
-- | Monadically iterate a system over all entities that have that component.
--   Note that writing to the store while iterating over it is undefined behaviour.
imapM_ :: forall w c. Has w c => (Entity c -> System w ()) -> System w ()
imapM_ sys = do s :: Storage c <- getStore
                explImapM_ s (sys . Entity)

{-# INLINE imapM #-}
-- | Monadically iterate a system over all entities that have that component.
--   Note that writing to the store while iterating over it is undefined behaviour.
imapM :: forall w c a. Has w c
      => (Entity c -> System w a) -> System w [a]
imapM sys = do s :: Storage c <- getStore
               explImapM s (sys . Entity)

-- | Maps a pure function over all components
{-# INLINE cmap #-}
cmap :: forall world c. Has world c => (c -> c) -> System world ()
cmap f = do s :: Storage c <- getStore
            liftIO$ explCmap s f

-- | 'mapM_' version of 'cmap'
{-# INLINE cmapM_ #-}
cmapM_ :: forall w c. Has w c => (c -> System w ()) -> System w ()
cmapM_ sys = do s :: Storage c <- getStore
                explCmapM_ s sys

-- | indexed 'cmapM_', also gives the current entity.
{-# INLINE cimapM_ #-}
cimapM_ :: forall w c. Has w c => ((Entity c, c) -> System w ()) -> System w ()
cimapM_ sys = do s :: Storage c <- getStore
                 explCimapM_ s (\(e,c) -> sys (Entity e,c))

-- | mapM version of cmap. Can be used to get a list of entities
--   As the type signature implies, and unlike 'cmap', the return value is not written to the component store.
{-# INLINE cmapM #-}
cmapM :: forall w c a. Has w c => (c -> System w a) -> System w [a]
cmapM sys = do s :: Storage c <- getStore
               explCmapM s sys

-- | indexed 'cmapM', also gives the current entity.
{-# INLINE cimapM #-}
cimapM :: forall w c a. Has w c => ((Entity c, c) -> System w a) -> System w [a]
cimapM sys = do s :: Storage c <- getStore
                explCimapM s (\(e,c) -> sys (Entity e,c))

-- | Maps a function that might delete its components
{-# INLINE cmap' #-}
cmap' :: forall world c. Has world c => (c -> Safe c) -> System world ()
cmap' f = do s :: Storage c <- getStore
             liftIO$ do sl <- explMembers s
                        U.forM_ sl $ \e -> do
                          r <- explGetUnsafe s e
                          explSetMaybe s e (getSafe . f $ r)

-- | Maps a function over all entities with a @r@, and writes their @w@
{-# INLINE rmap #-}
rmap :: forall world r w. (Has world w, Has world r)
      => (r -> w) -> System world ()
rmap f = do sr :: Storage r <- getStore
            sc :: Storage w <- getStore
            liftIO$ do sl <- explMembers sr
                       U.forM_ sl $ \ e -> do
                          r <- explGetUnsafe sr e
                          explSet sc e (f r)

-- | Maps a function over all entities with a @r@, and writes or deletes their @w@
{-# INLINE rmap' #-}
rmap' :: forall world r w. (Has world w, Has world r, Store (Storage r), Store (Storage w))
      => (r -> Safe w) -> System world ()
rmap' f = do sr :: Storage r <- getStore
             sw :: Storage w <- getStore
             liftIO$ do sl <- explMembers sr
                        U.forM_ sl $ \ e -> do
                           r <- explGetUnsafe sr e
                           explSetMaybe sw e (getSafe $ f r)

-- | For all entities with a @w@, this map reads their @r@ and writes their @w@
{-# INLINE wmap #-}
wmap :: forall world r w. (Has world w, Has world r, Store (Storage r), Store (Storage w))
     => (Safe r -> w) -> System world ()
wmap f = do sr :: Storage r <- getStore
            sw :: Storage w <- getStore
            liftIO$ do sl <- explMembers sr
                       U.forM_ sl $ \ e -> do
                         r <- explGet sr e
                         explSet sw e (f . Safe $ r)

-- | For all entities with a @w@, this map reads their @r@ and writes or deletes their @w@
{-# INLINE wmap' #-}
wmap' :: forall world r w. (Has world w, Has world r, Store (Storage r), Store (Storage w))
      => (Safe r -> Safe w) -> System world ()
wmap' f = do sr :: Storage r <- getStore
             sw :: Storage w <- getStore
             liftIO$ do sl <- explMembers sr
                        U.forM_ sl $ \ e -> do
                          r <- explGet sr e
                          explSetMaybe sw e (getSafe . f . Safe $ r)

-- | Reads a global value
{-# INLINE getGlobal #-}
getGlobal :: forall w c. (Has w c, GlobalStore (Storage c)) => System w c
getGlobal = do s :: Storage c <- getStore
               liftIO$ explGet s 0

-- | Writes a global value
{-# INLINE setGlobal #-}
setGlobal :: forall w c. (Has w c, GlobalStore (Storage c)) => c -> System w ()
setGlobal c = do s :: Storage c <- getStore
                 liftIO$ explSet s 0 c

-- | Modifies a global value
{-# INLINE modifyGlobal #-}
modifyGlobal :: forall w c. (Has w c, GlobalStore (Storage c)) => (c -> c) -> System w ()
modifyGlobal f = getGlobal >>= setGlobal . f

