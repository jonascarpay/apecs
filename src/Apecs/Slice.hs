{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

-- | This module is designed to be imported with qualified
module Apecs.Slice where

import qualified Data.Vector.Unboxed as U
import Data.Traversable (for)
import Control.Monad.IO.Class

import Apecs.Types

-- | Slice version of foldM_
{-# INLINE foldM_ #-}
foldM_ :: (a -> Entity c -> System w a) -> a -> Slice b -> System w ()
foldM_ f seed (Slice sl) = U.foldM'_ ((.Entity) . f) seed sl

-- | Gets the size of a slice (O(n))
{-# INLINE size #-}
size :: Slice a -> Int
size (Slice vec) = U.length vec

-- | Tests whether a slice is empty (O(1))
{-# INLINE null #-}
null :: Slice a -> Bool
null (Slice vec) = U.null vec

-- | Construct a slice from a list of IDs
{-# INLINE fromList #-}
fromList :: [Int] -> Slice a
fromList = Slice . U.fromList

-- | Monadically filter a slice
{-# INLINE filterM #-}
filterM :: (Entity c -> System w Bool) -> Slice c -> System w (Slice c)
filterM fm (Slice vec) = Slice <$> U.filterM (fm . Entity) vec

-- | Concatenates two slices. Equivalent to mappend
{-# INLINE concat #-}
concat :: Slice a -> Slice b -> Slice c
concat (Slice a) (Slice b) = Slice (a U.++ b)

-- Slice traversal
-- | Slice version of forM_
{-# INLINE forM_ #-}
forM_ :: Monad m => Slice c -> (Entity c -> m b) -> m ()
forM_ (Slice vec) ma = U.forM_ vec (ma . Entity)

-- | Slice version of forM
{-# INLINE forM #-}
forM :: Monad m => Slice c -> (Entity c -> m a) -> m [a]
forM (Slice vec) ma = traverse (ma . Entity) (U.toList vec)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE forMC #-}
forMC :: forall w c a. Has w c => Slice c -> ((Entity c,Safe c) -> System w a) -> System w [a]
forMC (Slice vec) sys = do
  s :: Storage c <- getStore
  for (U.toList vec) $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE forMC_ #-}
forMC_ :: forall w c a. Has w c => Slice c -> ((Entity c,Safe c) -> System w a) -> System w ()
forMC_ (Slice vec) sys = do
  s :: Storage c <- getStore
  U.forM_ vec $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

-- | Slice version of mapM_
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (Entity c -> m a) -> Slice c -> m ()
mapM_ ma (Slice vec) = U.mapM_ (ma . Entity) vec

-- | Slice version of mapM
{-# INLINE mapM #-}
mapM :: Monad m => (Entity c -> m a) -> Slice c -> m [a]
mapM ma (Slice vec) = traverse (ma . Entity) (U.toList vec)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE mapMC #-}
mapMC :: forall w c a. Has w c => ((Entity c,Safe c) -> System w a) -> Slice c -> System w [a]
mapMC sys (Slice vec) = do
  s :: Storage c <- getStore
  for (U.toList vec) $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE mapMC_ #-}
mapMC_ :: forall w c a. Has w c => ((Entity c, Safe c) -> System w a) -> Slice c -> System w ()
mapMC_ sys vec = forMC_ vec sys

toList :: Slice a -> [Entity a]
toList (Slice sl) = Entity <$> U.toList sl
