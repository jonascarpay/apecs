{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Apecs.Stores
  ( Map, Set, Flag(..), Cache, Unique,
    Global,
    Cachable,
  ) where

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as VM
import Control.Monad.Reader
import GHC.TypeLits
import Data.Proxy

import Apecs.Types

{-# INLINE defaultSetMaybe #-}
defaultSetMaybe :: (EntityStore s, SafeRW s ~ Maybe (Stores s)) => s -> Int -> Maybe (Stores s) -> IO ()
defaultSetMaybe s e Nothing  = explDestroy s e
defaultSetMaybe s e (Just c) = explSet s e c

-- | A map from Data.Intmap.Strict. O(n log(n)) for most operations.
--   Yields safe runtime representations of type @Maybe c@.
newtype Map c = Map (IORef (M.IntMap c))
instance ComponentStore (Map c) where
  type InitArgs (Map c) = ()
  type Stores (Map c) = c
  initStoreWith _ = Map <$> newIORef mempty
instance EntityStore (Map c) where
  explDestroy (Map ref) ety = modifyIORef' ref (M.delete ety)
  explMembers (Map ref)     = U.fromList . M.keys <$> readIORef ref
  explExists  (Map ref) ety = M.member ety <$> readIORef ref
  explReset   (Map ref)     = writeIORef ref mempty
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  type SafeRW (Map c) = Maybe c
  explGetUnsafe (Map ref) ety = fromJust . M.lookup ety <$> readIORef ref
  explGet       (Map ref) ety = M.lookup ety <$> readIORef ref
  explSet       (Map ref) ety x = modifyIORef' ref $ M.insert ety x
  explSetMaybe = defaultSetMaybe
  explModify    (Map ref) ety f = modifyIORef' ref $ M.adjust f ety
  explCmap      (Map ref) f = modifyIORef' ref $ M.map f
  explCmapM_    (Map ref) ma = liftIO (readIORef ref) >>= mapM_ ma
  explCmapM     (Map ref) ma = liftIO (readIORef ref) >>= mapM  ma . M.elems
  explCimapM_   (Map ref) ma = liftIO (readIORef ref) >>= mapM_ ma . M.assocs
  explCimapM    (Map ref) ma = liftIO (readIORef ref) >>= mapM  ma . M.assocs
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}
  {-# INLINE explCmapM_ #-}
  {-# INLINE explCmapM #-}
  {-# INLINE explCimapM_ #-}
  {-# INLINE explCimapM #-}

-- | Class for flags, used by @Set@ to yield runtime representations.
class Flag c where
  flag :: c

-- | A store that keeps membership, but holds no values.
--   Produces @flag@ runtime values.
newtype Set c = Set (IORef S.IntSet)
instance ComponentStore (Set c) where
  type InitArgs (Set c) = ()
  type Stores (Set c) = c
  initStoreWith _ = Set <$> newIORef mempty
instance Flag c => EntityStore (Set c) where
  explDestroy (Set ref) ety = modifyIORef' ref (S.delete ety)
  explMembers (Set ref) = U.fromList . S.toList <$> readIORef ref
  explReset (Set ref) = writeIORef ref mempty
  explExists (Set ref) ety = S.member ety <$> readIORef ref
  explImapM_  (Set ref) ma = liftIO (readIORef ref) >>= mapM_ ma . S.toList
  explImapM   (Set ref) ma = liftIO (readIORef ref) >>= mapM  ma . S.toList
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  {-# INLINE explImapM_ #-}
  {-# INLINE explImapM #-}
  type SafeRW (Set c) = Bool
  explGetUnsafe _ _ = return flag
  explGet (Set ref) ety = S.member ety <$> readIORef ref
  explSet (Set ref) ety _ = modifyIORef' ref $ S.insert ety
  explSetMaybe s ety False = explDestroy s ety
  explSetMaybe s ety True  = explSet s ety flag
  explCmap _ _ = return ()
  explModify _ _ _ = return ()
  explCmapM   = error "Iterating over set"
  explCmapM_  = error "Iterating over set"
  explCimapM  = error "Iterating over set"
  explCimapM_ = error "Iterating over set"
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}

-- | A Unique contains exactly one component belonging to some entity.
--   Writing to it overwrites both the previous component and its owner.
data Unique c = Unique (IORef Int) (IORef c)
instance ComponentStore (Unique c) where
  type InitArgs (Unique c) = ()
  type Stores (Unique c) = c
  initStoreWith _ = Unique <$> newIORef (-1) <*> newIORef undefined
instance EntityStore (Unique c) where
  explDestroy (Unique eref _) ety = do e <- readIORef eref; when (e==ety) (writeIORef eref (-1))
  explMembers (Unique eref _) = U.singleton <$> readIORef eref
  explReset   (Unique eref _) = writeIORef eref (-1)
  explExists  (Unique eref _) ety = (==ety) <$> readIORef eref
  explImapM_  (Unique eref _) ma = do e <- liftIO (readIORef eref); when (e /= -1) (void$ ma e)
  explImapM   (Unique eref _) ma = do
    e <- liftIO (readIORef eref)
    if e /= -1 then return [] else pure <$> ma e
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  {-# INLINE explImapM_ #-}
  {-# INLINE explImapM #-}

  type SafeRW (Unique c) = Maybe c
  explGetUnsafe (Unique _ cref) _ = readIORef cref
  explGet       (Unique eref cref) ety = do
    e <- readIORef eref
    if e == ety then Just <$> readIORef cref else return Nothing

  explSet       (Unique eref cref) ety x = writeIORef eref ety >> writeIORef cref x
  explSetMaybe = defaultSetMaybe
  explCmap      (Unique _    cref) f = modifyIORef' cref f
  explModify    (Unique eref cref) ety f = do
    e <- readIORef eref
    when (e==ety) (modifyIORef' cref f)

  explCmapM (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    if e /= -1 then liftIO (readIORef cref) >>= fmap pure . ma
               else return []

  explCmapM_ (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    when (e /= -1) . void $ liftIO (readIORef cref) >>= ma

  explCimapM (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    if e /= -1 then liftIO (readIORef cref) >>= fmap pure . ma . (,) e
               else return []

  explCimapM_ (Unique eref cref) ma = do
    e <- liftIO$ readIORef eref
    when (e /= -1) . void $ liftIO (readIORef cref) >>= ma . (,) e

  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}


-- | Constant value. Not very practical, but fun to write.
newtype Const c = Const c
instance ComponentStore (Const c) where
  type InitArgs (Const c) = c
  type Stores (Const c) = c
  initStoreWith c = return$ Const c
instance GlobalStore (Const c) c where
  explGlobalRead  (Const c) = return c
  explGlobalWrite  _ _ = return ()
  explGlobalModify _ _ = return ()
instance EntityStore (Const c) where
  explDestroy _ _ = return ()
  explExists  _ _  = return False
  explMembers _ = return mempty
  explReset _ = return ()
  type SafeRW (Const c) = c
  explGetUnsafe (Const c) _ = return c
  explGet       (Const c) _ = return c
  explSet       _ _ _ = return ()
  explSetMaybe  _ _ _ = return ()
  explModify    _ _ _ = return ()
  explCmap       _ _ = return ()

-- | Global value.
--   Must be given an initial value upon construction.
newtype Global c = Global (IORef c)
instance ComponentStore (Global c) where
  type InitArgs (Global c) = c
  type Stores   (Global c) = c
  initStoreWith c = Global <$> newIORef c

instance GlobalStore (Global c) c where
  explGlobalRead   (Global ref) = readIORef    ref
  explGlobalWrite  (Global ref) = writeIORef   ref
  explGlobalModify (Global ref) = modifyIORef' ref
  {-# INLINE explGlobalRead #-}
  {-# INLINE explGlobalWrite #-}
  {-# INLINE explGlobalModify #-}

-- | A cache around another store.
--   The wrapped store must produce safe representations using Maybe.
--   Note that iterating over a cache is linear in its size, so large, sparsely populated caches will actually decrease performance.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Stores s)) s

class (ComponentStore s, EntityStore s, EntityStore s, SafeRW s ~ Maybe (Stores s)) => Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

instance (KnownNat n, Cachable s) => ComponentStore (Cache n s) where
  type InitArgs (Cache n s) = (InitArgs s)
  type Stores (Cache n s) = Stores s
  initStoreWith args = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStoreWith args
    return (Cache n tags cache child)

instance Cachable s => EntityStore (Cache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `mod` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety then return True else explExists s ety

  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

  {-# INLINE explReset #-}
  explReset (Cache n tags _ s) = do
    forM_ [0..n-1] $ \e -> UM.write tags e (-1)
    explReset s

  {-# INLINE explImapM_ #-}
  explImapM_ (Cache _ tags _ s) ma = do
    liftIO (U.freeze tags) >>= U.mapM_ ma . U.filter (/= (-1))
    explImapM_ s ma

  {-# INLINE explImapM #-}
  explImapM (Cache _ tags _ s) ma = do
    as1 <- liftIO (U.freeze tags) >>= mapM ma . U.toList . U.filter (/= (-1))
    as2 <- explImapM s ma
    return (as1 ++ as2)

  type SafeRW (Cache n s) = SafeRW s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Cache n tags cache s) ety = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGetUnsafe s ety

  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) ety = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then Just <$> VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

  {-# INLINE explSetMaybe #-}
  explSetMaybe = defaultSetMaybe

  {-# INLINE explCmap #-}
  explCmap (Cache n tags cache s) f = do
    forM_ [0..n-1] $ \e -> do
      tag <- UM.read tags e
      unless (tag == (-1)) (VM.modify cache f e)
    explCmap s f

  {-# INLINE explModify #-}
  explModify (Cache n tags cache s) ety f = do
    let index = ety `mod` n
    tag <- UM.read tags index
    if tag == ety
       then VM.modify cache f ety
       else explModify s ety f

  {-# INLINE explCmapM_ #-}
  explCmapM_ (Cache n tags cache s) ma = do
    forM_ [0..n-1] $ \e -> do
      tag <- liftIO$ UM.read tags e
      unless (tag == (-1)) $ do
        r <- liftIO$ VM.read cache e
        void$ ma r
    explCmapM_ s ma

  {-# INLINE explCimapM_ #-}
  explCimapM_ (Cache n tags cache s) ma = do
    forM_ [0..n-1] $ \e -> do
      tag <- liftIO$ UM.read tags e
      unless (tag == (-1)) $ do
        r <- liftIO$ VM.read cache e
        void$ ma (e, r)
    explCimapM_ s ma
