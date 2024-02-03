{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- For Data.Semigroup compatibility

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Util (
  -- * Utility
  runGC, global,

  -- * EntityCounter
  EntityCounter(..), nextEntity, newEntity, newEntity_,
) where

import           Control.Applicative  (liftA2)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Semigroup
import           System.Mem           (performMajorGC)

import           Apecs.Core
import           Apecs.Stores
import           Apecs.System

-- | Convenience entity, for use in places where the entity value does not matter, i.e. a global store.
global :: Entity
global = Entity (-1)

-- | Component used by newEntity to track the number of issued entities.
--   Automatically added to any world created with @makeWorld@
newtype EntityCounter = EntityCounter {getCounter :: Sum Int} deriving (Semigroup, Monoid, Eq, Show)

instance Component EntityCounter where
  type Storage EntityCounter = ReadOnly (Global EntityCounter)

-- | Bumps the EntityCounter and yields its value
{-# INLINE nextEntity #-}
nextEntity :: (MonadIO m, Get w m EntityCounter) => SystemT w m Entity
nextEntity = do EntityCounter n <- get global
                setReadOnly global (EntityCounter $ n+1)
                return (Entity . getSum $ n)

-- | Writes the given components to a new entity, and yields that entity.
-- The return value is often ignored.
{-# INLINE newEntity #-}
newEntity :: (MonadIO m, Set w m c, Get w m EntityCounter)
          => c -> SystemT w m Entity
newEntity c = do ety <- nextEntity
                 set ety c
                 return ety

-- | Writes the given components to a new entity without yelding the result.
-- Used mostly for convenience.
{-# INLINE newEntity_ #-}
newEntity_ :: (MonadIO m, Set world m component, Get world m EntityCounter)
           => component -> SystemT world m ()
newEntity_ component = do
  entity <- nextEntity
  set entity component

-- | Explicitly invoke the garbage collector
runGC :: MonadIO m => SystemT w m ()
runGC = liftIO performMajorGC
