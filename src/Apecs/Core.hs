{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Apecs.Core where


import Control.Monad
import Control.Monad.IO.Class
import Data.Traversable (for)
import qualified Data.Vector.Unboxed as U

type ID = Int
type IDVec = U.Vector ID
newtype Slice c = Slice {unSlice :: U.Vector ID} deriving (Show, Monoid)
newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Ord, Show)

-- Storage type class hierarchy
-- | Common for every storage. Represents a container that can be initialized
class Initializable s where
  type InitArgs s
  initStoreWith :: InitArgs s -> IO s

-- | A store that is indexed by entities
class HasMembers s where
  explDestroy :: s -> Int -> IO ()
  explExists  :: s -> Int -> IO Bool
  explMembers :: s -> IO (U.Vector Int)

  {-# INLINE explReset #-}
  explReset :: s -> IO ()
  explReset s = do
    sl <- explMembers s
    U.mapM_ (explDestroy s) sl

  explImapM_ :: MonadIO m => s -> (Int -> m a) -> m ()
  {-# INLINE explImapM_ #-}
  explImapM_ s ma = liftIO (explMembers s) >>= Prelude.mapM_ ma . U.toList

  explImapM :: MonadIO m => s -> (Int -> m a) -> m [a]
  {-# INLINE explImapM #-}
  explImapM s ma = liftIO (explMembers s) >>= Prelude.mapM ma . U.toList

-- | Class of storages that associates components with entities.
class HasMembers s => Store s where
  type SafeRW s -- ^ Return type for safe reads/writes to the store
  type Stores s -- ^ The type of components stored by this Store
  -- | Unsafe index to the store. Undefined if the component does not exist
  explGetUnsafe :: s -> Int -> IO (Stores s)
  -- | Retrieves a component from the store
  explGet       :: s -> Int -> IO (SafeRW s)
  -- | Writes a component
  explSet       :: s -> Int -> Stores s -> IO ()
  -- | Either writes or deletes a component
  explSetMaybe  :: s -> Int -> SafeRW s -> IO ()

  -- | Modifies an element in the store.
  {-# INLINE explModify #-}
  explModify :: s -> Int -> (Stores s -> Stores s) -> IO ()
  explModify s ety f = do etyExists <- explExists s ety
                          when etyExists $ explGetUnsafe s ety >>= explSet s ety . f

  -- | Maps over all elements of this store.
  --   The default implementation can be replaced by an optimized one
  explCmap :: s -> (Stores s -> Stores s) -> IO ()
  {-# INLINE explCmap #-}
  explCmap s f = do
    sl <- explMembers s
    U.forM_ sl $ \ety -> do
      x :: Stores s <- explGetUnsafe s ety
      explSet s ety (f x)

  explCmapM_ :: MonadIO m => s -> (Stores s -> m a) -> m ()
  {-# INLINE explCmapM_ #-}
  explCmapM_ s sys = do
    sl <- liftIO$ explMembers s
    U.forM_ sl $ \ety -> do x :: Stores s <- liftIO$ explGetUnsafe s ety
                            sys x

  explCimapM_ :: MonadIO m => s -> ((Int, Stores s) -> m a) -> m ()
  {-# INLINE explCimapM_ #-}
  explCimapM_ s sys = do
    sl <- liftIO$ explMembers s
    U.forM_ sl $ \ety -> do x :: Stores s <- liftIO$ explGetUnsafe s ety
                            sys (ety,x)

  explCmapM  :: MonadIO m => s -> (Stores s -> m a) -> m [a]
  {-# INLINE explCmapM #-}
  explCmapM s sys = do
    sl <- liftIO$ explMembers s
    for (U.toList sl) $ \ety -> do
      x :: Stores s <- liftIO$ explGetUnsafe s ety
      sys x

  explCimapM :: MonadIO m => s -> ((Int, Stores s) -> m a) -> m [a]
  {-# INLINE explCimapM #-}
  explCimapM s sys = do
    sl <- liftIO$ explMembers s
    for (U.toList sl) $ \ety -> do
      x :: Stores s <- liftIO$ explGetUnsafe s ety
      sys (ety,x)

-- | Class of storages for global values
class GlobalRW s c where
  {-# MINIMAL explGlobalRead, explGlobalWrite #-}
  explGlobalRead :: s -> IO c
  explGlobalWrite :: s -> c -> IO ()

  {-# INLINE explGlobalModify #-}
  explGlobalModify :: s -> (c -> c) -> IO ()
  explGlobalModify s f = do r <- explGlobalRead s
                            explGlobalWrite s (f r)

-- Query
class Query q s where
  explSlice :: s -> q -> IO (U.Vector Int)

data All = All
instance HasMembers s => Query All s where
  {-# INLINE explSlice #-}
  explSlice s _ = explMembers s

class Cast a b where cast :: a -> b
instance Cast (Entity a) (Entity b) where
  {-# INLINE cast #-}
  cast (Entity ety) = Entity ety
instance Cast (Slice a) (Slice b) where
  {-# INLINE cast #-}
  cast (Slice vec) = Slice vec

-- Tuple Instances
-- (,)
instance (Initializable a, Initializable b) => Initializable (a,b) where
  type InitArgs (a, b) = (InitArgs a, InitArgs b)
  initStoreWith (aa, ab) = (,) <$> initStoreWith aa <*> initStoreWith ab

instance (HasMembers a, HasMembers b) => HasMembers (a,b) where
  explMembers (sa,sb) = explMembers sa >>= U.filterM (explExists sb)
  explReset   (sa,sb) = explReset sa >> explReset sb
  explDestroy (sa,sb) ety = explDestroy sa ety >> explDestroy sb ety
  explExists  (sa,sb) ety = (&&) <$> explExists sa ety <*> explExists sb ety
  {-# INLINE explMembers #-}
  {-# INLINE explReset #-}
  {-# INLINE explDestroy #-}
  {-# INLINE explExists #-}

instance (Store a, Store b) => Store (a, b) where
  type SafeRW (a, b) = (SafeRW a, SafeRW b)
  type Stores (a, b) = (Stores a, Stores b)
  explGetUnsafe  (sa,sb) ety = (,) <$> explGetUnsafe sa ety <*> explGetUnsafe sb ety
  explGet        (sa,sb) ety = (,) <$> explGet sa ety <*> explGet sb ety
  explSet        (sa,sb) ety (wa,wb) = explSet sa ety wa >> explSet sb ety wb
  explSetMaybe   (sa,sb) ety (wa,wb) = explSetMaybe sa ety wa >> explSetMaybe sb ety wb
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}

instance (GlobalRW a ca, GlobalRW b cb) => GlobalRW (a,b) (ca,cb) where
  explGlobalRead  (sa,sb) = (,) <$> explGlobalRead sa <*> explGlobalRead sb
  explGlobalWrite (sa,sb) (wa,wb) = explGlobalWrite sa wa >> explGlobalWrite sb wb
  {-# INLINE explGlobalRead #-}
  {-# INLINE explGlobalWrite #-}

-- (,,)
instance (Initializable a, Initializable b, Initializable c) => Initializable (a,b,c) where
  type InitArgs (a, b, c) = (InitArgs a, InitArgs b, InitArgs c)
  initStoreWith (aa, ab, ac) = (,,) <$> initStoreWith aa <*> initStoreWith ab <*> initStoreWith ac

instance (HasMembers a, HasMembers b, HasMembers c) => HasMembers (a,b,c) where
  explMembers (sa,sb,sc) = explMembers sa >>= U.filterM (explExists sb) >>= U.filterM (explExists sc)
  explReset   (sa,sb,sc) = explReset sa >> explReset sb >> explReset sc
  explDestroy (sa,sb,sc) ety = explDestroy sa ety >> explDestroy sb ety >> explDestroy sc ety
  explExists  (sa,sb,sc) ety = and <$> sequence [explExists sa ety, explExists sb ety, explExists sc ety]
  {-# INLINE explMembers #-}
  {-# INLINE explReset #-}
  {-# INLINE explDestroy #-}
  {-# INLINE explExists #-}

instance (Store a, Store b, Store c) => Store (a, b, c) where
  type SafeRW (a, b, c) = (SafeRW a, SafeRW b, SafeRW c)
  type Stores (a, b, c) = (Stores a, Stores b, Stores c)
  explGetUnsafe  (sa,sb,sc) ety = (,,) <$> explGetUnsafe sa ety <*> explGetUnsafe sb ety <*> explGetUnsafe sc ety
  explGet        (sa,sb,sc) ety = (,,) <$> explGet sa ety <*> explGet sb ety <*> explGet sc ety
  explSet        (sa,sb,sc) ety (wa,wb,wc) = explSet sa ety wa >> explSet sb ety wb >> explSet sc ety wc
  explSetMaybe   (sa,sb,sc) ety (wa,wb,wc) = explSetMaybe sa ety wa >> explSetMaybe sb ety wb >> explSetMaybe sc ety wc
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}

instance (GlobalRW a ca, GlobalRW b cb, GlobalRW c cc) => GlobalRW (a,b,c) (ca,cb,cc) where
  explGlobalRead  (sa,sb,sc) = (,,) <$> explGlobalRead sa <*> explGlobalRead sb <*> explGlobalRead sc
  explGlobalWrite (sa,sb,sc) (wa,wb,wc) = explGlobalWrite sa wa >> explGlobalWrite sb wb >> explGlobalWrite sc wc
  {-# INLINE explGlobalRead #-}
  {-# INLINE explGlobalWrite #-}
