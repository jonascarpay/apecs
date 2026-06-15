{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| This module contains STM-supporting versions of regular apecs stores, and some convenience functions.
It is designed to be imported qualified, since it shadows both apecs and STM names.
There is also an @Apecs.STM.Prelude@ module, which can be imported by itself.

Note that if you want to be able to create entities in STM, you will also need to use a STM-supported @EntityCounter@, typically done through this module's @makeWorld@.
-}
module Apecs.STM
  ( -- * Stores
    Map (..)
  , TMap (..)
  , Sharded
  , Unique (..)
  , Global (..)

    -- * EntityCounter
  , EntityCounter (..)
  , nextEntity
  , newEntity
  , newEntity_
  , makeWorld
  , makeWorldAndComponents

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
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Typeable (Typeable, typeRep)
import qualified Data.Vector.Unboxed as U
import GHC.Conc (unsafeIOToSTM)
import Language.Haskell.TH
import qualified ListT as L
import qualified StmContainers.Map as M

import Apecs (Sharded, ask, runSystem, set)
import Apecs.Core
import Apecs.Stores (Cachable)
import Apecs.TH (makeMapComponentsFor, makeWorldNoEC)
import Apecs.Util (EntityCounter (..), nextEntityIO)

newtype Map c = Map (M.Map Int c)
type instance Elem (Map c) = c

instance ExplInit STM (Map c) where
  explInit = Map <$> M.new
instance (Typeable c) => ExplGet STM (Map c) where
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}
  explExists (Map m) ety = isJust <$> M.lookup ety m
  explGet (Map m) ety = flip fmap (M.lookup ety m) $ \case
    Just c -> c
    notFound ->
      error $
        unwords
          [ "Reading non-existent STM Map component"
          , show (typeRep notFound)
          , "for entity"
          , show ety
          ]

instance ExplSet STM (Map c) where
  {-# INLINE explSet #-}
  explSet (Map m) ety x = M.insert x ety m
instance ExplDestroy STM (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map m) ety = M.delete ety m
instance ExplMembers STM (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map m) = U.unfoldrM L.uncons $ fst <$> M.listT m
  {-# INLINE explMemberSet #-}
  explMemberSet (Map m) = IS.fromList . U.toList <$> explMembers (Map m)

instance ExplInit IO (Map c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance (Typeable c) => ExplGet IO (Map c) where
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}
  explExists m e = S.atomically $ explExists m e
  explGet m e = S.atomically $ explGet m e
instance ExplSet IO (Map c) where
  {-# INLINE explSet #-}
  explSet m e x = S.atomically $ explSet m e x
instance ExplDestroy IO (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy m e = S.atomically $ explDestroy m e
instance ExplMembers IO (Map c) where
  {-# INLINE explMembers #-}
  explMembers m = S.atomically $ explMembers m
  {-# INLINE explMemberSet #-}
  explMemberSet m = S.atomically $ explMemberSet m

{- | A map store backed by a @TVar (IntMap (TVar c))@: an outer 'TVar' holding
the set of live entities, and a per-entity inner 'TVar' holding each component
value.

This two-level structure is what makes it concurrency-friendly. Reading or
writing the value of an /existing/ entity only touches that entity's inner
'TVar' (the outer 'TVar' is merely read), so two transactions updating
different existing components never conflict. Only structural changes —
inserting a new entity or destroying one — write the outer 'TVar'.

Each operation is a cheap 'TVar' access over a pure
'Data.IntMap.Strict.IntMap', so per-operation cost is far lower than the
stm-containers backed 'Map'. The trade-off is that all structural changes, and
'explMembers', contend on the single outer 'TVar'; wrap a 'TMap' in
'Apecs.Sharded' to spread that contention across shards:

> instance Component Pos where
>   type Storage Pos = Sharded 64 (TMap Pos)
-}
newtype TMap c = TMap (TVar (IM.IntMap (TVar c)))

type instance Elem (TMap c) = c

instance Cachable (TMap c)

instance ExplInit STM (TMap c) where
  explInit = TMap <$> newTVar IM.empty
instance (Typeable c) => ExplGet STM (TMap c) where
  {-# INLINE explExists #-}
  explExists (TMap ref) ety = IM.member ety <$> readTVar ref
  {-# INLINE explGet #-}
  explGet (TMap ref) ety = do
    m <- readTVar ref
    case IM.lookup ety m of
      Just cell -> readTVar cell
      notFound ->
        error $
          unwords
            [ "Reading non-existent STM TMap component"
            , show (typeRep notFound)
            , "for entity"
            , show ety
            ]

instance ExplSet STM (TMap c) where
  {-# INLINE explSet #-}
  explSet (TMap ref) ety x = do
    m <- readTVar ref
    case IM.lookup ety m of
      Just cell -> writeTVar cell x
      Nothing -> do
        cell <- newTVar x
        writeTVar ref $! IM.insert ety cell m
instance ExplDestroy STM (TMap c) where
  {-# INLINE explDestroy #-}
  explDestroy (TMap ref) ety = modifyTVar' ref (IM.delete ety)
instance ExplMembers STM (TMap c) where
  {-# INLINE explMembers #-}
  explMembers (TMap ref) = U.fromList . IM.keys <$> readTVar ref
  {-# INLINE explMemberSet #-}
  explMemberSet (TMap ref) = IM.keysSet <$> readTVar ref

instance ExplInit IO (TMap c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance (Typeable c) => ExplGet IO (TMap c) where
  {-# INLINE explExists #-}
  explExists m e = S.atomically $ explExists m e
  {-# INLINE explGet #-}
  explGet m e = S.atomically $ explGet m e
instance ExplSet IO (TMap c) where
  {-# INLINE explSet #-}
  explSet m e x = S.atomically $ explSet m e x
instance ExplDestroy IO (TMap c) where
  {-# INLINE explDestroy #-}
  explDestroy m e = S.atomically $ explDestroy m e
instance ExplMembers IO (TMap c) where
  {-# INLINE explMembers #-}
  explMembers m = S.atomically $ explMembers m
  {-# INLINE explMemberSet #-}
  explMemberSet m = S.atomically $ explMemberSet m

newtype Unique c = Unique (TVar (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance ExplInit STM (Unique c) where
  explInit = Unique <$> newTVar Nothing

instance (Typeable c) => ExplGet STM (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readTVar ref) $ \case
    Just (_, c) -> c
    notFound ->
      error $
        unwords
          [ "Reading non-existent STM Unique component"
          , show (typeRep notFound)
          ]
  {-# INLINE explExists #-}
  explExists (Unique ref) ety = maybe False ((== ety) . fst) <$> readTVar ref

instance ExplSet STM (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = writeTVar ref (Just (ety, c))

instance ExplDestroy STM (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety =
    readTVar ref
      >>= mapM_ (flip when (writeTVar ref Nothing) . (== ety) . fst)

instance ExplMembers STM (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = flip fmap (readTVar ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety
  {-# INLINE explMemberSet #-}
  explMemberSet (Unique ref) = flip fmap (readTVar ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> IS.singleton ety

instance ExplInit IO (Unique c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance (Typeable c) => ExplGet IO (Unique c) where
  {-# INLINE explExists #-}
  explExists m e = S.atomically $ explExists m e
  {-# INLINE explGet #-}
  explGet m e = S.atomically $ explGet m e
instance ExplSet IO (Unique c) where
  {-# INLINE explSet #-}
  explSet m e x = S.atomically $ explSet m e x
instance ExplDestroy IO (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy m e = S.atomically $ explDestroy m e
instance ExplMembers IO (Unique c) where
  {-# INLINE explMembers #-}
  explMembers m = S.atomically $ explMembers m
  {-# INLINE explMemberSet #-}
  explMemberSet m = S.atomically $ explMemberSet m

newtype Global c = Global (TVar c)
type instance Elem (Global c) = c
instance (Monoid c) => ExplInit STM (Global c) where
  {-# INLINE explInit #-}
  explInit = Global <$> newTVar mempty
instance ExplGet STM (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = readTVar ref
  {-# INLINE explExists #-}
  explExists _ _ = return True
instance ExplSet STM (Global c) where
  {-# INLINE explSet #-}
  explSet (Global ref) _ c = writeTVar ref c

instance (Monoid c) => ExplInit IO (Global c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance ExplGet IO (Global c) where
  {-# INLINE explExists #-}
  explExists m e = S.atomically $ explExists m e
  {-# INLINE explGet #-}
  explGet m e = S.atomically $ explGet m e
instance ExplSet IO (Global c) where
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

-- | Like @makeWorld@ from @Apecs@, but uses the STM @EntityCounter@
makeWorld :: String -> [Name] -> Q [Dec]
makeWorld worldName cTypes = makeWorldNoEC worldName (cTypes ++ [''EntityCounter])

-- | Like @makeWorldAndComponents@ from @Apecs@, but uses the STM @EntityCounter@ and the STM @Map@
makeWorldAndComponents :: String -> [Name] -> Q [Dec]
makeWorldAndComponents worldName cTypes = do
  wdecls <- makeWorld worldName cTypes
  cdecls <- makeMapComponentsFor ''Map cTypes
  pure $ wdecls ++ cdecls

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
