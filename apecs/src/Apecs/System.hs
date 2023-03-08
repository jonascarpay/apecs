{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}

module Apecs.System where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Proxy
import qualified Data.Vector.Unboxed  as U

import Apecs.Components ()
import Apecs.Core

-- | Run a system in a game world
{-# INLINE runSystem #-}
runSystem :: SystemT w m a -> w -> m a
runSystem sys = runReaderT (unSystem sys)

-- | Run a system in a game world
{-# INLINE runWith #-}
runWith :: w -> SystemT w m a -> m a
runWith = flip runSystem

-- | Read a Component
{-# INLINE get #-}
get :: forall w m c. Get w m c => Entity -> SystemT w m c
get (Entity ety) = do
  s :: Storage c <- getStore
  lift$ explGet s ety

-- | Writes a Component to a given Entity. Will overwrite existing Components.
{-# INLINE set #-}
set, ($=) :: forall w m c. Set w m c => Entity -> c -> SystemT w m ()
set (Entity ety) x = do
  s :: Storage c <- getStore
  lift$ explSet s ety x

-- | @set@ operator
($=) = set
infixr 2 $=

-- | Returns whether the given entity has component @c@
{-# INLINE exists #-}
exists :: forall w m c. Get w m c => Entity -> Proxy c -> SystemT w m Bool
exists (Entity ety) _ = do
  s :: Storage c <- getStore
  lift$ explExists s ety

-- | Destroys component @c@ for the given entity.
{-# INLINE destroy #-}
destroy :: forall w m c. Destroy w m c => Entity -> Proxy c -> SystemT w m ()
destroy (Entity ety) ~_ = do
  s :: Storage c <- getStore
  lift$ explDestroy s ety

-- | Applies a function, if possible.
{-# INLINE modify #-}
modify, ($~) :: forall w m cx cy. (Get w m cx, Set w m cy) => Entity -> (cx -> cy) -> SystemT w m ()
modify (Entity ety) f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  lift$ do
    possible <- explExists sx ety
    when possible $ do
      x <- explGet sx ety
      explSet sy ety (f x)

-- | @modify@ operator
($~) = modify
infixr 2 $~

-- | Maps a function over all entities with a @cx@, and writes their @cy@.
{-# INLINE cmap #-}
cmap :: forall w m cx cy. (Get w m cx, Members w m cx, Set w m cy)
     => (cx -> cy) -> SystemT w m ()
cmap f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  lift$ do
    sl <- explMembers sx
    U.forM_ sl $ \ e -> do
      r <- explGet sx e
      explSet sy e (f r)

-- | Conditional @cmap@, that first tests whether the argument satisfies some property.
--   The entity needs to have both a cx and cp component.
{-# INLINE cmapIf #-}
cmapIf :: forall w m cp cx cy.
  ( Get w m cx
  , Get w m cp
  , Members w m cx
  , Set w m cy )
  => (cp -> Bool)
  -> (cx -> cy)
  -> SystemT w m ()
cmapIf cond f = do
  sp :: Storage cp <- getStore
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  lift$ do
    sl <- explMembers (sx,sp)
    U.forM_ sl $ \ e -> do
      p <- explGet sp e
      when (cond p) $ do
        x <- explGet sx e
        explSet sy e (f x)

-- | Monadically iterates over all entites with a @cx@, and writes their @cy@.
{-# INLINE cmapM #-}
cmapM :: forall w m cx cy. (Get w m cx, Set w m cy, Members w m cx)
      => (cx -> SystemT w m cy) -> SystemT w m ()
cmapM sys = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  sl <- lift$ explMembers sx
  U.forM_ sl $ \ e -> do
    x <- lift$ explGet sx e
    y <- sys x
    lift$ explSet sy e y

-- | Monadically iterates over all entites with a @cx@
{-# INLINE cmapM_ #-}
cmapM_ :: forall w m c. (Get w m c, Members w m c)
       => (c -> SystemT w m ()) -> SystemT w m ()
cmapM_ sys = do
  s :: Storage c <- getStore
  sl <- lift$ explMembers s
  U.forM_ sl $ \ ety -> do
    x <- lift$ explGet s ety
    sys x

-- | Fold over the game world; for example, @cfold max (minBound :: Foo)@ will find the maximum value of @Foo@.
--   Strict in the accumulator.
{-# INLINE cfold #-}
cfold :: forall w m c a. (Members w m c, Get w m c)
      => (a -> c -> a) -> a -> SystemT w m a
cfold f a0 = do
  s :: Storage c <- getStore
  sl <- lift$ explMembers s
  lift$ U.foldM' (\a e -> f a <$> explGet s e) a0 sl

-- | Monadically fold over the game world.
--   Strict in the accumulator.
{-# INLINE cfoldM #-}
cfoldM :: forall w m c a. (Members w m c, Get w m c)
       => (a -> c -> SystemT w m a) -> a -> SystemT w m a
cfoldM sys a0 = do
  s :: Storage c <- getStore
  sl <- lift$ explMembers s
  U.foldM' (\a e -> lift (explGet s e) >>= sys a) a0 sl

-- | Monadically fold over the game world.
--   Strict in the accumulator.
{-# INLINE cfoldM_ #-}
cfoldM_ :: forall w m c a. (Members w m c, Get w m c)
       => (a -> c -> SystemT w m a) -> a -> SystemT w m ()
cfoldM_ sys a0 = do
  s :: Storage c <- getStore
  sl <- lift$ explMembers s
  U.foldM'_ (\a e -> lift (explGet s e) >>= sys a) a0 sl

-- | Collect matching components into a list by using the specified test/process function.
--   You can use this to preprocess data before returning.
--   And you can do a test here that depends on data from multiple components.
--   Pass "Just" to simply collect all the items.
{-# INLINE collect #-}
collect :: forall components w m a. (Get w m components, Members w m components)
        => (components -> Maybe a)
        -> SystemT w m [a]
collect f = cfold (\acc -> maybe acc (: acc) . f) []
