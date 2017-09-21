{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Apecs.Stores
  ( Map, Set, Flag(..), Cache, Unique,
    Global,
    Log(..), PureLog(..), FromPure(..), Logger, getLog,
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

-- | A map from Data.Intmap.Strict. O(n log(n)) for most operations.
--   Yields safe runtime representations of type @Maybe c@.
newtype Map c = Map (IORef (M.IntMap c))
instance Initializable (Map c) where
  type InitArgs (Map c) = ()
  initStoreWith _ = Map <$> newIORef mempty
instance HasMembers (Map c) where
  explDestroy (Map ref) ety = modifyIORef' ref (M.delete ety)
  explMembers (Map ref)     = U.fromList . M.keys <$> readIORef ref
  explExists  (Map ref) ety = M.member ety <$> readIORef ref
  explReset   (Map ref)     = writeIORef ref mempty
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
instance Store (Map c) where
  type SafeRW (Map c) = Maybe c
  type Stores (Map c) = c
  explGetUnsafe (Map ref) ety = fromJust . M.lookup ety <$> readIORef ref
  explGet       (Map ref) ety = M.lookup ety <$> readIORef ref
  explSet       (Map ref) ety x = modifyIORef' ref $ M.insert ety x
  explSetMaybe  s ety Nothing = explDestroy s ety
  explSetMaybe  s ety (Just x) = explSet s ety x
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
instance Initializable (Set c) where
  type InitArgs (Set c) = ()
  initStoreWith _ = Set <$> newIORef mempty
instance HasMembers (Set c) where
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
instance (Flag c) => Store (Set c) where
  type SafeRW (Set c) = Bool
  type Stores (Set c) = c
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
instance Initializable (Unique c) where
  type InitArgs (Unique c) = ()
  initStoreWith _ = Unique <$> newIORef (-1) <*> newIORef undefined
instance HasMembers (Unique c) where
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

instance Store (Unique c) where
  type SafeRW (Unique c) = Maybe c
  type Stores (Unique c) = c
  explGetUnsafe (Unique _ cref) _ = readIORef cref
  explGet       (Unique eref cref) ety = do
    e <- readIORef eref
    if e == ety then Just <$> readIORef cref else return Nothing

  explSet       (Unique eref cref) ety x = writeIORef eref ety >> writeIORef cref x
  explSetMaybe  s ety Nothing = explDestroy s ety
  explSetMaybe  s ety (Just x) = explSet s ety x
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
instance Initializable (Const c) where
  type InitArgs (Const c) = c
  initStoreWith c = return$ Const c
instance GlobalRW (Const c) c where
  explGlobalRead  (Const c) = return c
  explGlobalWrite  _ _ = return ()
  explGlobalModify _ _ = return ()
instance HasMembers (Const c) where
  explDestroy _ _ = return ()
  explExists  _ _  = return False
  explMembers _ = return mempty
  explReset _ = return ()
instance Store (Const c) where
  type SafeRW (Const c) = c
  type Stores (Const c) = c
  explGetUnsafe (Const c) _ = return c
  explGet       (Const c) _ = return c
  explSet       _ _ _ = return ()
  explSetMaybe  _ _ _ = return ()
  explModify    _ _ _ = return ()
  explCmap       _ _ = return ()

-- | Global value.
--   Must be given an initial value upon construction.
newtype Global c = Global (IORef c)
instance Initializable (Global c) where
  type InitArgs (Global c) = c
  initStoreWith c = Global <$> newIORef c

instance GlobalRW (Global c) c where
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

class (Initializable s, HasMembers s, Store s, SafeRW s ~ Maybe (Stores s)) => Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

instance (KnownNat n, Cachable s) => Initializable (Cache n s) where
  type InitArgs (Cache n s) = (InitArgs s)
  initStoreWith args = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStoreWith args
    return (Cache n tags cache child)

instance Cachable s => HasMembers (Cache n s) where
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

instance Cachable s => Store (Cache n s) where
  type SafeRW (Cache n s) = SafeRW s
  type Stores (Cache n s) = Stores s

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
  explSetMaybe c ety Nothing  = explDestroy c ety
  explSetMaybe c ety (Just x) = explSet c ety x

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

-- | A PureLog is a piece of state @l c@ that is updated when components @c@ are written or destroyed.
--   Note that @l :: * -> *@
class PureLog l c where
  logEmpty :: l c
  logOnSet :: Entity a -> Maybe c -> c -> l c -> l c
  logOnDestroy :: Entity a -> c -> l c -> l c

-- | An Log is a PureLog with mutable state.
class Log l c where
  ioLogEmpty     :: IO (l c)
  ioLogOnSet     :: l c -> Entity a -> Maybe c -> c -> IO ()
  ioLogOnDestroy :: l c -> Entity a -> c -> IO ()
  ioLogReset     :: l c -> IO ()

class HasLog s l where
  explGetLog :: s -> l (Stores s)

{-instance HasLog s l => HasLog (Logger a s) l where-}
  {-{-# INLINE explGetLog #-}-}
  {-explGetLog (Logger _ s) = explGetLog s-}

instance HasLog (Logger l s) l where
  {-# INLINE explGetLog #-}
  explGetLog (Logger l _) = l

getLog :: forall w c l. (IsRuntime c, Has w c, HasLog (Storage c) l, Log l c) => System w (l c)
getLog = do s :: Storage c <- getStore
            return (explGetLog s)


-- | FromPure turns a PureLog into a Log
newtype FromPure l c = FromPure (IORef (l c))
instance PureLog l c => Log (FromPure l) c where
  {-# INLINE ioLogEmpty #-}
  ioLogEmpty = FromPure <$> newIORef logEmpty
  {-# INLINE ioLogOnSet #-}
  ioLogOnSet (FromPure lref) e old new = modifyIORef' lref (logOnSet e old new)
  {-# INLINE ioLogOnDestroy #-}
  ioLogOnDestroy (FromPure lref) e c = modifyIORef' lref (logOnDestroy e c)
  {-# INLINE ioLogReset #-}
  ioLogReset (FromPure lref) = writeIORef lref logEmpty

-- | A Logger l of some store updates the Log l with the writes and deletes to Store s
data Logger l s = Logger (l (Stores s)) s

instance (Log l (Stores s), Cachable s) => Initializable (Logger l s) where
  type InitArgs (Logger l s) = InitArgs s
  initStoreWith args = Logger <$> ioLogEmpty <*> initStoreWith args

instance (Log l (Stores s), Cachable s) => HasMembers (Logger l s) where
  {-# INLINE explDestroy #-}
  explDestroy (Logger l s) ety = do
    mc <- explGet s ety
    case mc of
      Just c -> ioLogOnDestroy l (Entity ety) c >> explDestroy s ety
      _ -> return ()

  {-# INLINE explExists #-}
  explExists (Logger _ s) ety = explExists s ety
  {-# INLINE explMembers #-}
  explMembers (Logger _ s) = explMembers s
  {-# INLINE explReset #-}
  explReset (Logger l s) = ioLogReset l >> explReset s
  {-# INLINE explImapM_ #-}
  explImapM_ (Logger _ s) = explImapM_ s
  {-# INLINE explImapM #-}
  explImapM (Logger _ s) = explImapM s

instance (Log l (Stores s), Cachable s) => Store (Logger l s) where
  type SafeRW (Logger l s) = SafeRW s
  type Stores (Logger l s) = Stores s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Logger _ s) ety = explGetUnsafe s ety
  {-# INLINE explGet #-}
  explGet (Logger _ s) ety = explGet s ety
  {-# INLINE explSet #-}
  explSet (Logger l s) ety x = do
    mc <- explGet s ety
    ioLogOnSet l (Entity ety) mc x
    explSet s ety x

  {-# INLINE explSetMaybe #-}
  explSetMaybe s ety (Nothing) = explDestroy s ety
  explSetMaybe s ety (Just x) = explSet s ety x

  {-# INLINE explModify #-}
  explModify (Logger l s) ety f = do
    mc <- explGet s ety
    case mc of
      Just c -> explSet (Logger l s) ety (f c)
      Nothing -> return ()

  {-# INLINE explCmapM_ #-}
  explCmapM_  (Logger _ s) = explCmapM_  s
  {-# INLINE explCmapM #-}
  explCmapM   (Logger _ s) = explCmapM   s
  {-# INLINE explCimapM_ #-}
  explCimapM_ (Logger _ s) = explCimapM_ s
  {-# INLINE explCimapM #-}
  explCimapM  (Logger _ s) = explCimapM  s
