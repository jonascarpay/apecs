{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | A collection of default stores in the IO monad.
module Apecs.Stores
  ( EntityCounter (..),
    newEntity,
    Map (..),
    Global (..),
    Unique (..),
    Cacheable,
    Cache (..),
    ReadOnly (..),
    forceReadonly,
  )
where

import Apecs.Core
import Apecs.Focus
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask, withReaderT)
import Data.Bits
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Proxy
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import GHC.Generics
import GHC.TypeNats

-- Map
newtype Map c = Map {getMap :: IORef (IM.IntMap c)}

type instance Components (Map c) = '[c]

instance MonadIO m => Get (Map c) m c where
  get (Entity ety) = do
    Map ref <- ask
    liftIO $ (IM.! ety) <$> readIORef ref
  exists _ (Entity ety) = do
    Map ref <- ask
    liftIO $ IM.member ety <$> readIORef ref

instance MonadIO m => Set (Map c) m c where
  set c (Entity ety) = do
    Map ref <- ask
    liftIO $ modifyIORef' ref (IM.insert ety c)

instance MonadIO m => Destroy (Map c) m c where
  destroy _ (Entity ety) = do
    Map ref <- ask
    liftIO $ modifyIORef' ref (IM.delete ety)

instance MonadIO m => Members (Map c) m c where
  members _ = do
    Map ref <- ask
    etys <- liftIO $ fmap Entity . IM.keys <$> readIORef ref
    pure $ VS.fromList etys

instance MonadIO m => Initialize m (Map c) where
  initialize = liftIO $ Map <$> newIORef mempty

-- Global
newtype Global c = Global {getGlobal :: IORef c}

type instance Components (Global c) = '[c]

instance MonadIO m => Get (Global c) m c where
  exists _ _ = pure True
  get _ = do
    Global ref <- ask
    liftIO $ readIORef ref

instance MonadIO m => Set (Global c) m c where
  set s _ = do
    Global ref <- ask
    liftIO $ writeIORef ref s

instance (Monoid c, MonadIO m) => Initialize m (Global c) where
  initialize = Global <$> liftIO (newIORef mempty)

-- Unique
newtype Unique c = Unique {getUnique :: IORef (Maybe (Entity, c))}

type instance Components (Unique c) = '[c]

instance MonadIO m => Get (Unique c) m c where
  exists _ ety = do
    Unique ref <- ask
    r <- liftIO $ readIORef ref
    pure $ case r of
      Just (n, _) | n == ety -> True
      _ -> False

  get _ = do
    Unique ref <- ask
    r <- liftIO $ readIORef ref
    pure $ case r of
      Just (_, c) -> c
      _ -> error "Invalid `get` from Unique"

instance MonadIO m => Set (Unique c) m c where
  set s ety = do
    Unique ref <- ask
    liftIO $ writeIORef ref (Just (ety, s))

instance MonadIO m => Initialize m (Unique c) where
  initialize = Unique <$> liftIO (newIORef Nothing)

-- Cache
class Cacheable s c

instance Cacheable (Map c) c

instance Cacheable s c => Cacheable (Cache n s c) c

data Cache (n :: Nat) s c = Cache
  { _cacheBitMask :: Int,
    _cacheTags :: VSM.IOVector Entity,
    _cacheMembers :: VM.IOVector c,
    _cachee :: s
  }

{-# INLINE withCached #-}
withCached :: SystemT s m a -> SystemT (Cache n s c) m a
withCached = withReaderT _cachee

cacheMiss :: t
cacheMiss = error "Cache miss! If you are seeing this during normal operation, please open a bug report at https://github.com/jonascarpay/apecs"

type instance Components (Cache n s c) = Components s

instance (MonadIO m, Initialize m s, KnownNat n, Cacheable s c) => Initialize m (Cache n s c) where
  initialize = do
    let n = fromIntegral $ natVal (Proxy @n) :: Int
        size = head . dropWhile (< n) $ iterate (`shiftL` 1) 1
        mask = size - 1
    tags <- liftIO $ VSM.replicate size (Entity (-2))
    cache <- liftIO $ VM.replicate size cacheMiss
    child <- initialize
    return (Cache mask tags cache child)
  {-# INLINE initialize #-}

instance {-# OVERLAPPING #-} (MonadIO m, Get s m c) => Get (Cache n s c) m c where
  {-# INLINE get #-}
  get ety = do
    (Cache mask tags cache _) <- ask
    let index = unEntity ety .&. mask
    tag <- liftIO $ VSM.unsafeRead tags index
    if tag == ety
      then liftIO $ VM.unsafeRead cache index
      else withCached (get ety)

  {-# INLINE exists #-}
  exists p ety = do
    Cache mask tags _ _ <- ask
    tag <- liftIO $ VSM.unsafeRead tags (unEntity ety .&. mask)
    if tag == ety
      then return True
      else withCached (exists p ety)

instance {-# OVERLAPPING #-} (MonadIO m, Set s m c) => Set (Cache n s c) m c where
  {-# INLINE set #-}
  set c ety = do
    Cache mask tags cache _ <- ask
    let index = unEntity ety .&. mask
    tag <- liftIO $ VSM.unsafeRead tags index
    when (tag /= Entity (-2) && tag /= ety) $ do
      cached <- liftIO $ VM.unsafeRead cache index
      withCached $ set cached tag
    liftIO $ VSM.unsafeWrite tags index ety
    liftIO $ VM.unsafeWrite cache index c

instance (MonadIO m, Destroy s m c) => Destroy (Cache n s c) m c where
  {-# INLINE destroy #-}
  destroy p ety = do
    Cache mask tags cache _ <- ask
    let index = unEntity ety .&. mask
    tag <- liftIO $ VSM.unsafeRead tags mask
    when (tag == ety) $ liftIO $ do
      VSM.unsafeWrite tags index (Entity (-2))
      VM.unsafeWrite cache index cacheMiss
    withCached $ destroy p ety

instance (MonadIO m, Members s m c) => Members (Cache n s c) m c where
  {-# INLINE members #-}
  members p = do
    (Cache mask tags _ _) <- ask
    cached <- liftIO $ VS.filter (/= Entity (-2)) <$> VS.freeze tags
    let etyFilter ety = (/= ety) <$> liftIO (VSM.unsafeRead tags (unEntity ety .&. mask))
    stored <- withCached (members p) >>= liftIO . VS.filterM etyFilter
    return $! cached VS.++ stored

instance {-# OVERLAPPABLE #-} Get s m c => Get (Cache n s c') m c where
  {-# INLINE get #-}
  get = withCached . get
  {-# INLINE exists #-}
  exists p = withCached . exists p

instance {-# OVERLAPPABLE #-} Set s m c => Set (Cache n s c') m c where
  {-# INLINE set #-}
  set c = withCached . set c

instance {-# OVERLAPPABLE #-} Destroy s m c => Destroy (Cache n s c') m c where
  {-# INLINE destroy #-}
  destroy p = withCached . destroy p

instance {-# OVERLAPPABLE #-} Members s m c => Members (Cache n s c') m c where
  {-# INLINE members #-}
  members p = withCached $ members p

-- ReadOnly
newtype ReadOnly s = ReadOnly {getReadOnly :: s}
  deriving (Generic)

{-# INLINE forceReadonly #-}
forceReadonly :: SystemT s m c -> SystemT (ReadOnly s) m c
forceReadonly = withReaderT getReadOnly

type instance Components (ReadOnly s) = Components s

instance Get s m c => Get (ReadOnly s) m c where
  get = forceReadonly . get
  exists p = forceReadonly . exists p

instance Members s m c => Members (ReadOnly s) m c where
  members = forceReadonly . members

instance (Initialize m s, Functor m) => Initialize m (ReadOnly s)

-- EntityCounter
newtype EntityCounter = EntityCounter (IORef Entity)

type instance Components EntityCounter = '[]

instance MonadIO m => Initialize m EntityCounter where
  initialize = liftIO $ EntityCounter <$> newIORef (Entity 0)

{-# INLINE newEntity #-}
newEntity ::
  ( Generic w,
    HasField w EntityCounter,
    MonadIO m,
    Set w m c
  ) =>
  c ->
  SystemT w m Entity
newEntity c = do
  EntityCounter ref <- focusField ask
  ety <- liftIO $ readIORef ref
  set c ety
  liftIO $ writeIORef ref (Entity . (+ 1) . unEntity $ ety)
  pure ety
