{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Apecs.Stores
  ( EntityCounter (..),
    newEntity,

    -- * Simple Stores
    IM.IntMap,
    Global (..),
    Unique (..),

    -- * Modifiers
    IOStore (..),
    newEntityIO,
    Cacheable,
    Cache (..),
    ReadOnly (..),
    forceReadonly,
  )
where

import Apecs.Core
import Apecs.Focus
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.State as S
import Data.Bits
import Data.Functor.Identity
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Proxy
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import GHC.Generics
import GHC.TypeNats

-- Map
type instance Components (IM.IntMap c) = '[c]

instance Monad m => Get (IM.IntMap c) m c where
  {-# INLINE get #-}
  get (Entity ety) = S.gets (IM.! ety)
  {-# INLINE exists #-}
  exists _ (Entity ety) = S.gets (IM.member ety)

instance Monad m => Set (IM.IntMap c) m c where
  {-# INLINE set #-}
  set c (Entity ety) = S.modify (IM.insert ety c)

instance Monad m => Destroy (IM.IntMap c) m c where
  {-# INLINE destroy #-}
  destroy _ (Entity ety) = S.modify (IM.delete ety)

instance Monad m => Members (IM.IntMap c) m c where
  {-# INLINE members #-}
  members _ = S.gets $ VS.fromList . fmap Entity . IM.keys

instance Applicative m => Initialize m (IM.IntMap c) where
  initialize = pure mempty

-- Global
newtype Global c = Global {unGlobal :: c}

type instance Components (Global c) = '[c]

instance Monad m => Get (Global c) m c where
  {-# INLINE exists #-}
  exists _ _ = pure True
  {-# INLINE get #-}
  get _ = S.gets unGlobal

instance Monad m => Set (Global c) m c where
  {-# INLINE set #-}
  set s _ = S.put $ Global s

instance (Monoid c, Applicative m) => Initialize m (Global c) where
  initialize = pure $ Global mempty

-- Unique
newtype Unique c = Unique {getUnique :: Maybe (Entity, c)}

type instance Components (Unique c) = '[c]

instance Monad m => Get (Unique c) m c where
  {-# INLINE exists #-}
  exists _ ety = S.gets $ \case
    Unique (Just (n, _)) | n == ety -> True
    _ -> False

  {-# INLINE get #-}
  get _ = S.gets $ \case
    Unique (Just (_, c)) -> c
    _ -> error "get: getting from empty Unique"

instance Monad m => Set (Unique c) m c where
  {-# INLINE set #-}
  set s ety = S.put $ Unique $ Just (ety, s)

instance Monad m => Initialize m (Unique c) where
  initialize = pure $ Unique Nothing

-- ReadOnly
newtype ReadOnly s = ReadOnly {getReadOnly :: s}
  deriving (Generic)

{-# INLINE forceReadonly #-}
forceReadonly :: Functor m => SystemT s m c -> SystemT (ReadOnly s) m c
forceReadonly = zoom l
  where
    l f (ReadOnly s) = ReadOnly <$> f s

type instance Components (ReadOnly s) = Components s

instance (Functor m, Get s m c) => Get (ReadOnly s) m c where
  {-# INLINE get #-}
  get = forceReadonly . get
  {-# INLINE exists #-}
  exists p = forceReadonly . exists p

instance (Functor m, Members s m c) => Members (ReadOnly s) m c where
  {-# INLINE members #-}
  members = forceReadonly . members

instance (Initialize m s, Functor m) => Initialize m (ReadOnly s)

-- EntityCounter
newtype EntityCounter = EntityCounter Entity

type instance Components EntityCounter = '[]

instance Monad m => Initialize m EntityCounter where
  initialize = pure $ EntityCounter (Entity 0)

{-# INLINE newEntity #-}
newEntity ::
  ( Generic w,
    Monad m,
    HasField w EntityCounter,
    Set w m c
  ) =>
  c ->
  SystemT w m Entity
newEntity c = do
  EntityCounter (Entity ety) <- focusField S.get
  set c (Entity ety)
  focusField $ S.put $ EntityCounter $ Entity (ety + 1)
  pure $ Entity ety

{-# INLINE newEntityIO #-}
newEntityIO ::
  ( Generic w,
    MonadIO m,
    HasField w (IOStore EntityCounter),
    Set w m c
  ) =>
  c ->
  SystemT w m Entity
newEntityIO c = do
  IOStore ref <- focusField S.get
  ety <- liftIO $ do
    EntityCounter (Entity ety) <- readIORef ref
    writeIORef ref $ EntityCounter (Entity (ety + 1))
    pure (Entity ety)
  set c ety
  pure ety

newtype IOStore s = IOStore (IORef s)

type instance Components (IOStore s) = Components s

instance (Get s Identity c, MonadIO m) => Get (IOStore s) m c where
  {-# INLINE exists #-}
  exists _ ety = do
    IOStore ref <- S.get
    S.evalState (exists (Proxy @c) ety) <$> liftIO (readIORef ref)
  {-# INLINE get #-}
  get ety = do
    IOStore ref <- S.get
    S.evalState (get ety) <$> liftIO (readIORef ref)

instance (Set s Identity c, MonadIO m) => Set (IOStore s) m c where
  {-# INLINE set #-}
  set c ety = do
    IOStore ref <- S.get
    liftIO $
      readIORef ref >>= writeIORef ref . S.execState (set c ety)

instance (MonadIO m, Destroy s Identity c) => Destroy (IOStore s) m c where
  {-# INLINE destroy #-}
  destroy _ ety = do
    IOStore ref <- S.get
    liftIO $
      readIORef ref >>= writeIORef ref . S.execState (destroy (Proxy @c) ety)

instance (MonadIO m, Members s Identity c) => Members (IOStore s) m c where
  {-# INLINE members #-}
  members _ = do
    IOStore ref <- S.get
    S.evalState (members (Proxy @c)) <$> liftIO (readIORef ref)

instance (MonadIO m, Initialize Identity s) => Initialize m (IOStore s) where
  initialize = liftIO $ IOStore <$> newIORef (runIdentity initialize)

-- Cache
class Cacheable s c

instance Cacheable s c => Cacheable (IOStore s) c

instance Cacheable (IM.IntMap c) c

instance Cacheable s c => Cacheable (Cache n s c) c

data Cache (n :: Nat) s c = Cache
  { _cacheBitMask :: Int,
    _cacheTags :: VSM.IOVector Entity,
    _cacheMembers :: VM.IOVector c,
    _cacheChild :: s
  }

{-# INLINE withCached #-}
withCached :: Functor m => SystemT s m a -> SystemT (Cache n s c) m a
withCached = zoom l
  where
    l f (Cache b t m c) = Cache b t m <$> f c

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
    (Cache mask tags cache _) <- S.get
    let index = unEntity ety .&. mask
    tag <- liftIO $ VSM.unsafeRead tags index
    if tag == ety
      then liftIO $ VM.unsafeRead cache index
      else withCached (get ety)

  {-# INLINE exists #-}
  exists p ety = do
    Cache mask tags _ _ <- S.get
    tag <- liftIO $ VSM.unsafeRead tags (unEntity ety .&. mask)
    if tag == ety
      then return True
      else withCached (exists p ety)

instance {-# OVERLAPPING #-} (MonadIO m, Set s m c) => Set (Cache n s c) m c where
  {-# INLINE set #-}
  set c ety = do
    Cache mask tags cache _ <- S.get
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
    Cache mask tags cache _ <- S.get
    let index = unEntity ety .&. mask
    tag <- liftIO $ VSM.unsafeRead tags mask
    when (tag == ety) $
      liftIO $ do
        VSM.unsafeWrite tags index (Entity (-2))
        VM.unsafeWrite cache index cacheMiss
    withCached $ destroy p ety

instance (MonadIO m, Members s m c) => Members (Cache n s c) m c where
  {-# INLINE members #-}
  members p = do
    (Cache mask tags _ _) <- S.get
    cached <- liftIO $ VS.filter (/= Entity (-2)) <$> VS.freeze tags
    let etyFilter ety = (/= ety) <$> liftIO (VSM.unsafeRead tags (unEntity ety .&. mask))
    stored <- withCached (members p) >>= liftIO . VS.filterM etyFilter
    return $! cached VS.++ stored

instance {-# OVERLAPPABLE #-} (Functor m, Get s m c) => Get (Cache n s c') m c where
  {-# INLINE get #-}
  get = withCached . get
  {-# INLINE exists #-}
  exists p = withCached . exists p

instance {-# OVERLAPPABLE #-} (Functor m, Set s m c) => Set (Cache n s c') m c where
  {-# INLINE set #-}
  set c = withCached . set c

instance {-# OVERLAPPABLE #-} (Functor m, Destroy s m c) => Destroy (Cache n s c') m c where
  {-# INLINE destroy #-}
  destroy p = withCached . destroy p

instance {-# OVERLAPPABLE #-} (Functor m, Members s m c) => Members (Cache n s c') m c where
  {-# INLINE members #-}
  members p = withCached $ members p
