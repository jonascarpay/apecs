{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | This module contains STM-supporting versions of regular apecs stores, and some convenience functions.
-- It is designed to be imported qualified, since it shadows both apecs and STM names.
-- There is also an @Apecs.STM.Prelude@ module, which can be imported by itself.
--
-- Note that if you want to be able to create entities in STM, you will also need to use a STM-supported @EntityCounter@, typically done through this module's @makeWorld@.

module Apecs.STM
  ( -- * Stores
    Map (..)
  , Unique (..)
  , Global (..)
    -- * STM conveniences
  , makeWorldAndComponents
  , atomically, retry, check, forkSys, threadDelay, STM
  ) where

import qualified Control.Concurrent          as S
import           Control.Concurrent.STM      (STM)
import qualified Control.Concurrent.STM      as S
import           Control.Concurrent.STM.TVar as S
import           Control.Monad
import           Data.Maybe
import           Data.Typeable (Typeable, typeRep)
import qualified Data.Vector.Unboxed         as U
import           Language.Haskell.TH
import qualified ListT                       as L
import qualified StmContainers.Map           as M

import           Apecs                       (ask, lift, liftIO, runSystem)
import           Apecs.Core
import           Apecs.TH                    (makeWorld, makeMapComponentsFor)

newtype Map c = Map (M.Map Int c)
type instance Elem (Map c) = c

instance ExplInit STM (Map c) where
  explInit = Map <$> M.new
instance Typeable c => ExplGet STM (Map c) where
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}
  explExists (Map m) ety = isJust   <$> M.lookup ety m
  explGet    (Map m) ety = flip fmap (M.lookup ety m) $ \case
    Just c -> c
    notFound -> error $ unwords
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

instance ExplInit IO (Map c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance Typeable c => ExplGet IO (Map c) where
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

newtype Unique c = Unique (TVar (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance ExplInit STM (Unique c) where
  explInit = Unique <$> newTVar Nothing

instance Typeable c => ExplGet STM (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readTVar ref) $ \case
    Just (_, c)  -> c
    notFound -> error $ unwords
      [ "Reading non-existent STM Unique component"
      , show (typeRep notFound)
      ]
  {-# INLINE explExists #-}
  explExists (Unique ref) ety = maybe False ((==ety) . fst) <$> readTVar ref

instance ExplSet STM (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = writeTVar ref (Just (ety, c))

instance ExplDestroy STM (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety = readTVar ref >>=
    mapM_ (flip when (writeTVar ref Nothing) . (==ety) . fst)

instance ExplMembers STM (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = flip fmap (readTVar ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

instance ExplInit IO (Unique c) where
  {-# INLINE explInit #-}
  explInit = S.atomically explInit
instance Typeable c => ExplGet IO (Unique c) where
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

newtype Global c = Global (TVar c)
type instance Elem (Global c) = c
instance Monoid c => ExplInit STM (Global c) where
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

instance Monoid c => ExplInit IO (Global c) where
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

-- | Like @makeWorldAndComponents@ from @Apecs@, but uses the STM 'Map'
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
