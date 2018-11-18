{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.STM where

import           Control.Concurrent.STM      as S
import           Control.Concurrent.STM.TVar as S
import           Control.Monad
import           Data.Maybe
import           Data.Monoid                 (Sum (..))
import qualified Data.Vector.Unboxed         as U
import qualified ListT                       as L
import qualified StmContainers.Map           as M

import           Apecs                       (get, global, set)
import           Apecs.Core

newtype Map c = Map (M.Map Int c)
type instance Elem (Map c) = c

instance ExplInit STM (Map c) where
  explInit = Map <$> M.new
instance ExplGet STM (Map c) where
  explExists (Map m) ety = isJust   <$> M.lookup ety m
  explGet    (Map m) ety = fromJust <$> M.lookup ety m
instance ExplSet STM (Map c) where
  explSet (Map m) ety x = M.insert x ety m
instance ExplDestroy STM (Map c) where
  explDestroy (Map m) ety = M.delete ety m
instance ExplMembers STM (Map c) where
  explMembers (Map m) = U.unfoldrM L.uncons $ fst <$> M.listT m

instance ExplInit IO (Map c) where
  explInit = atomically explInit
instance ExplGet IO (Map c) where
  explExists m e = atomically $ explExists m e
  explGet m e = atomically $ explGet m e
instance ExplSet IO (Map c) where
  explSet m e x = atomically $ explSet m e x
instance ExplDestroy IO (Map c) where
  explDestroy m e = atomically $ explDestroy m e
instance ExplMembers IO (Map c) where
  explMembers m = atomically $ explMembers m

newtype Unique c = Unique (TVar (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance ExplInit STM (Unique c) where
  explInit = Unique <$> newTVar Nothing

instance ExplGet STM (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readTVar ref) $ \case
    Nothing -> error "Reading empty Unique"
    Just (_, c)  -> c
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
  explInit = atomically explInit
instance ExplGet IO (Unique c) where
  explExists m e = atomically $ explExists m e
  explGet m e = atomically $ explGet m e
instance ExplSet IO (Unique c) where
  explSet m e x = atomically $ explSet m e x
instance ExplDestroy IO (Unique c) where
  explDestroy m e = atomically $ explDestroy m e
instance ExplMembers IO (Unique c) where
  explMembers m = atomically $ explMembers m


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
  explInit = atomically explInit
instance ExplGet IO (Global c) where
  explExists m e = atomically $ explExists m e
  explGet m e = atomically $ explGet m e
instance ExplSet IO (Global c) where
  explSet m e x = atomically $ explSet m e x

newtype EntityCounter = EntityCounter {getCounter :: Sum Int} deriving (Semigroup, Monoid, Eq, Show)

instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

{-# INLINE nextEntity #-}
nextEntity :: (Get w m EntityCounter, Set w m EntityCounter) => SystemT w m Entity
nextEntity = do EntityCounter n <- get global
                set global (EntityCounter $ n+1)
                return (Entity . getSum $ n)

{-# INLINE newEntity #-}
newEntity :: (Set w m c, Get w m EntityCounter, Set w m EntityCounter)
          => c -> SystemT w m Entity
newEntity c = do ety <- nextEntity
                 set ety c
                 return ety
