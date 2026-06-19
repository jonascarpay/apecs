{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| Stores and utilities that can partipicate in STM transactions.

Use sparingly!

Although this may be faster than always putting world under MVar,
grabbing too much entities and values in a single transaction would
incur quadratic costs.
-}
module Apecs.Stores.STM
  ( -- * Stores
    TMap (..)
  , TUnique (..)
  , TGlobal (..)

    -- * EntityCounter transactions
  , EntityCounter (..)
  , nextEntity
  , newEntity
  , newEntity_

    -- * STM conveniences
  , atomically
  , retry
  , check
  , forkSys
  , threadDelay
  , STM
  ) where

import qualified Control.Concurrent as S
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as S
import Control.Concurrent.STM.TVar as S
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Typeable (Typeable, typeRep)
import qualified Data.Vector.Unboxed as U
import GHC.Conc (unsafeIOToSTM)
import qualified ListT as L
import qualified StmContainers.Map as M

import Apecs (ask, runSystem, set)
import Apecs.Core
import Apecs.Util (EntityCounter (..), nextEntityIO)

newtype TMap c = TMap (M.Map Int c)
type instance Elem (TMap c) = c

instance ExplInit STM (TMap c) where
  explInit = TMap <$> M.new
instance (Typeable c) => ExplGet STM (TMap c) where
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}
  explExists (TMap m) ety = isJust <$> M.lookup ety m
  explGet (TMap m) ety = flip fmap (M.lookup ety m) $ \case
    Just c -> c
    notFound ->
      error $
        unwords
          [ "Reading non-existent STM Map component"
          , show (typeRep notFound)
          , "for entity"
          , show ety
          ]

instance ExplSet STM (TMap c) where
  {-# INLINE explSet #-}
  explSet (TMap m) ety x = M.insert x ety m
instance ExplDestroy STM (TMap c) where
  {-# INLINE explDestroy #-}
  explDestroy (TMap m) ety = M.delete ety m
instance ExplMembers STM (TMap c) where
  {-# INLINE explMembers #-}
  explMembers (TMap m) = U.unfoldrM L.uncons $ fst <$> M.listT m
  {-# INLINE explMemberSet #-}
  explMemberSet (TMap m) = IS.fromList <$> L.toList (fst <$> M.listT m)

instance ExplInit IO (TMap c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance (Typeable c) => ExplGet IO (TMap c) where
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}
  explExists m e = S.atomically $ explExists m e
  explGet m e = S.atomically $ explGet m e
instance ExplSet IO (TMap c) where
  {-# INLINE explSet #-}
  explSet m e x = S.atomically $ explSet m e x
instance ExplDestroy IO (TMap c) where
  {-# INLINE explDestroy #-}
  explDestroy m e = S.atomically $ explDestroy m e
instance ExplMembers IO (TMap c) where
  {-# INLINE explMembers #-}
  explMembers (TMap m) = U.unfoldrM L.uncons $ fst <$> M.listTNonAtomic m
  {-# INLINE explMemberSet #-}
  explMemberSet (TMap m) =
    L.fold (\a b -> pure $! IS.insert b a) IS.empty $ fst <$> M.listTNonAtomic m

newtype TUnique c = TUnique (TVar (Maybe (Int, c)))
type instance Elem (TUnique c) = c
instance ExplInit STM (TUnique c) where
  explInit = TUnique <$> newTVar Nothing

instance (Typeable c) => ExplGet STM (TUnique c) where
  {-# INLINE explGet #-}
  explGet (TUnique ref) _ = flip fmap (readTVar ref) $ \case
    Just (_, c) -> c
    notFound ->
      error $
        unwords
          [ "Reading non-existent STM TUnique component"
          , show (typeRep notFound)
          ]
  {-# INLINE explExists #-}
  explExists (TUnique ref) ety = maybe False ((== ety) . fst) <$> readTVar ref

instance ExplSet STM (TUnique c) where
  {-# INLINE explSet #-}
  explSet (TUnique ref) ety c = writeTVar ref (Just (ety, c))

instance ExplDestroy STM (TUnique c) where
  {-# INLINE explDestroy #-}
  explDestroy (TUnique ref) ety =
    readTVar ref
      >>= mapM_ (flip when (writeTVar ref Nothing) . (== ety) . fst)

instance ExplMembers STM (TUnique c) where
  {-# INLINE explMembers #-}
  explMembers (TUnique ref) = flip fmap (readTVar ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety
  {-# INLINE explMemberSet #-}
  explMemberSet (TUnique ref) = flip fmap (readTVar ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> IS.singleton ety

instance ExplInit IO (TUnique c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance (Typeable c) => ExplGet IO (TUnique c) where
  {-# INLINE explExists #-}
  explExists (TUnique ref) ety = maybe False ((== ety) . fst) <$> readTVarIO ref
  {-# INLINE explGet #-}
  explGet m e = S.atomically $ explGet m e
instance ExplSet IO (TUnique c) where
  {-# INLINE explSet #-}
  explSet m e x = S.atomically $ explSet m e x
instance ExplDestroy IO (TUnique c) where
  {-# INLINE explDestroy #-}
  explDestroy m e = S.atomically $ explDestroy m e
instance ExplMembers IO (TUnique c) where
  {-# INLINE explMembers #-}
  explMembers (TUnique ref) = flip fmap (readTVarIO ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety
  {-# INLINE explMemberSet #-}
  explMemberSet (TUnique ref) = flip fmap (readTVarIO ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> IS.singleton ety

newtype TGlobal c = TGlobal (TVar c)
type instance Elem (TGlobal c) = c
instance (Monoid c) => ExplInit STM (TGlobal c) where
  {-# INLINE explInit #-}
  explInit = TGlobal <$> newTVar mempty
instance ExplGet STM (TGlobal c) where
  {-# INLINE explGet #-}
  explGet (TGlobal ref) _ = readTVar ref
  {-# INLINE explExists #-}
  explExists _ _ = return True
instance ExplSet STM (TGlobal c) where
  {-# INLINE explSet #-}
  explSet (TGlobal ref) _ c = writeTVar ref c

instance (Monoid c) => ExplInit IO (TGlobal c) where
  {-# INLINE explInit #-}
  explInit = TGlobal <$> newTVarIO mempty
instance ExplGet IO (TGlobal c) where
  {-# INLINE explExists #-}
  explExists _ _ = return True
  {-# INLINE explGet #-}
  explGet (TGlobal ref) _ = readTVarIO ref
instance ExplSet IO (TGlobal c) where
  {-# INLINE explSet #-}
  explSet m e x = S.atomically $ explSet m e x

{-# INLINE nextEntity #-}
nextEntity :: (Has w IO EntityCounter) => SystemT w STM Entity
nextEntity = ask >>= lift . unsafeIOToSTM . runSystem nextEntityIO

{-# INLINE newEntity #-}
newEntity :: (Set w STM c, Has w IO EntityCounter) => c -> SystemT w STM Entity
newEntity c = do
  ety <- nextEntity
  set ety c
  return ety

{-# INLINE newEntity_ #-}
newEntity_ :: (Set w STM c, Has w IO EntityCounter) => c -> SystemT w STM ()
newEntity_ c = do
  ety <- nextEntity
  set ety c

-- -- | Like @makeWorld@ from @Apecs@, but uses the STM @EntityCounter@
-- makeWorld :: String -> [Name] -> Q [Dec]
-- makeWorld worldName cTypes = makeWorldNoEC worldName (cTypes ++ [''EntityCounter])

-- -- | Like @makeWorldAndComponents@ from @Apecs@, but uses the STM @EntityCounter@ and the STM @Map@
-- makeWorldAndComponents :: String -> [Name] -> Q [Dec]
-- makeWorldAndComponents worldName cTypes = do
--   wdecls <- makeWorld worldName cTypes
--   cdecls <- makeMapComponentsFor ''Map cTypes
--   pure $ wdecls ++ cdecls

-- | @atomically@ from STM, lifted to the System level.
atomically :: SystemT w STM a -> SystemT w IO a
atomically sys = ask >>= liftIO . S.atomically . runSystem sys

-- | @retry@ from STM, lifted to the System level.
retry :: SystemT w STM a
retry = lift S.retry

-- | @check@ from STM, lifted to the System level.
check :: Bool -> SystemT w STM ()
check = lift . S.check

-- | Runs a system on a new thread.
forkSys :: SystemT w IO () -> SystemT w IO S.ThreadId
forkSys sys = ask >>= liftIO . S.forkIO . runSystem sys

-- | Suspends the current thread for a number of microseconds.
threadDelay :: Int -> SystemT w IO ()
threadDelay = liftIO . S.threadDelay
