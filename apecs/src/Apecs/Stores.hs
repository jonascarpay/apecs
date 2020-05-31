{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Stores
  ( Map, Cache, Unique,
    Global,
    Cachable,
    ReadOnly, setReadOnly, destroyReadOnly
    -- Register, regLookup
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.IntMap.Strict     as M
import           Data.IORef
import           Data.Proxy
import           Data.Typeable          (Typeable, typeRep)
import qualified Data.Vector.Unboxed    as U

import           Apecs.Core
import           Apecs.Stores.Cache

-- | A map based on 'Data.IntMap.Strict'. O(log(n)) for most operations.
newtype Map c = Map (IORef (M.IntMap c))

type instance Elem (Map c) = c
instance MonadIO m => ExplInit m (Map c) where
  explInit = liftIO$ Map <$> newIORef mempty

instance (MonadIO m, Typeable c) => ExplGet m (Map c) where
  explExists (Map ref) ety = liftIO$ M.member ety <$> readIORef ref
  explGet    (Map ref) ety = liftIO$ flip fmap (M.lookup ety <$> readIORef ref) $ \case
    Just c -> c
    notFound -> error $ unwords
      [ "Reading non-existent Map component"
      , show (typeRep notFound)
      , "for entity"
      , show ety
      ]
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance MonadIO m => ExplSet m (Map c) where
  {-# INLINE explSet #-}
  explSet (Map ref) ety x = liftIO$
    modifyIORef' ref (M.insert ety x)

instance MonadIO m => ExplDestroy m (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map ref) ety = liftIO$
    readIORef ref >>= writeIORef ref . M.delete ety

instance MonadIO m => ExplMembers m (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map ref) = liftIO$ U.fromList . M.keys <$> readIORef ref

instance Cachable (Map s)

-- | A Unique contains zero or one component.
--   Writing to it overwrites both the previous component and its owner.
--   Its main purpose is to be a 'Map' optimized for when only ever one component inhabits it.
newtype Unique c = Unique (IORef (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance MonadIO m => ExplInit m (Unique c) where
  explInit = liftIO$ Unique <$> newIORef Nothing

instance (MonadIO m, Typeable c) => ExplGet m (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = liftIO$ flip fmap (readIORef ref) $ \case
    Just (_, c)  -> c
    notFound -> error $ unwords
      [ "Reading non-existent Unique component"
      , show (typeRep notFound)
      ]

  {-# INLINE explExists #-}
  explExists (Unique ref) ety = liftIO$ maybe False ((==ety) . fst) <$> readIORef ref

instance MonadIO m => ExplSet m (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = liftIO$ writeIORef ref (Just (ety, c))

instance MonadIO m => ExplDestroy m (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety = liftIO$ readIORef ref >>=
    mapM_ (flip when (writeIORef ref Nothing) . (==ety) . fst)

instance MonadIO m => ExplMembers m (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = liftIO$ flip fmap (readIORef ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

-- | A 'Global' contains exactly one component.
--   The initial value is 'mempty' from the component's 'Monoid' instance.
--   Querying a 'Global' at /any/ Entity yields this one component, effectively sharing the component between /all/ entities.
--
--   A Global component can be read with @'get' 0@ or @'get' 1@ or even @'get' undefined@.
--   The convenience entity 'global' is defined as -1, and can be used to make operations on a global more explicit, i.e. 'Time t <- get global'.
--
--   You also can read and write Globals during a 'cmap' over other components.
newtype Global c = Global (IORef c)
type instance Elem (Global c) = c
instance (Monoid c, MonadIO m) => ExplInit m (Global c) where
  {-# INLINE explInit #-}
  explInit = liftIO$ Global <$> newIORef mempty

instance MonadIO m => ExplGet m (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = liftIO$ readIORef ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance MonadIO m => ExplSet m (Global c) where
  {-# INLINE explSet #-}
  explSet (Global ref) _ c = liftIO$ writeIORef ref c

-- | Wrapper that makes a store read-only by hiding its 'ExplSet' and 'ExplDestroy' instances.
--   This is primarily used to protect the 'EntityCounter' from accidental overwrites.
--   Use 'setReadOnly' and 'destroyReadOnly' to override.
newtype ReadOnly s = ReadOnly s
type instance Elem (ReadOnly s) = Elem s

instance (Functor m, ExplInit m s) => ExplInit m (ReadOnly s) where
  explInit = ReadOnly <$> explInit

instance ExplGet m s => ExplGet m (ReadOnly s) where
  explExists (ReadOnly s) = explExists s
  explGet    (ReadOnly s) = explGet s
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplMembers m s => ExplMembers m (ReadOnly s) where
  {-# INLINE explMembers #-}
  explMembers (ReadOnly s) = explMembers s

setReadOnly :: forall w m s c.
  ( Has w m c
  , Storage c ~ ReadOnly s
  , Elem s ~ c
  , ExplSet m s
  ) => Entity -> c -> SystemT w m ()
setReadOnly (Entity ety) c = do
  ReadOnly s <- getStore
  lift $ explSet s ety c

destroyReadOnly :: forall w m s c.
  ( Has w m c
  , Storage c ~ ReadOnly s
  , Elem s ~ c
  , ExplDestroy m s
  ) => Entity -> Proxy c -> SystemT w m ()
destroyReadOnly (Entity ety) _ = do
  ReadOnly s :: Storage c <- getStore
  lift $ explDestroy s ety
