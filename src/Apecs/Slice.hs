{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Apecs.Slice where

import qualified Data.Vector.Unboxed as U
import Data.Traversable (for)
import Control.Monad.IO.Class

import Apecs.Types

-- | Slice version of foldM_
{-# INLINE sliceFoldM_ #-}
sliceFoldM_ :: (a -> Entity c -> System w a) -> a -> Slice b -> System w ()
sliceFoldM_ f seed (Slice sl) = U.foldM'_ ((.Entity) . f) seed sl

-- | Gets the size of a slice (O(n))
{-# INLINE sliceSize #-}
sliceSize :: Slice a -> Int
sliceSize (Slice vec) = U.length vec

-- | Tests whether a slice is empty (O(1))
{-# INLINE sliceNull #-}
sliceNull :: Slice a -> Bool
sliceNull (Slice vec) = U.null vec

-- | Construct a slice from a list of IDs
{-# INLINE sliceFromList #-}
sliceFromList :: [Int] -> Slice a
sliceFromList = Slice . U.fromList

-- | Monadically filter a slice
{-# INLINE sliceFilterM #-}
sliceFilterM :: (Entity c -> System w Bool) -> Slice c -> System w (Slice c)
sliceFilterM fm (Slice vec) = Slice <$> U.filterM (fm . Entity) vec

-- | Concatenates two slices. Equivalent to mappend
{-# INLINE sliceConcat #-}
sliceConcat :: Slice a -> Slice b -> Slice c
sliceConcat (Slice a) (Slice b) = Slice (a U.++ b)

-- Slice traversal
-- | Slice version of forM_
{-# INLINE sliceForM_ #-}
sliceForM_ :: Monad m => Slice c -> (Entity c -> m b) -> m ()
sliceForM_ (Slice vec) ma = U.forM_ vec (ma . Entity)

-- | Slice version of forM
{-# INLINE sliceForM #-}
sliceForM :: Monad m => Slice c -> (Entity c -> m a) -> m [a]
sliceForM (Slice vec) ma = traverse (ma . Entity) (U.toList vec)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE sliceForMC #-}
sliceForMC :: forall w c a. (Store (Storage c), Has w c) => Slice c -> ((Entity c,Safe c) -> System w a) -> System w [a]
sliceForMC (Slice vec) sys = do
  s :: Storage c <- getStore
  for (U.toList vec) $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE sliceForMC_ #-}
sliceForMC_ :: forall w c a. (Store (Storage c), Has w c) => Slice c -> ((Entity c,Safe c) -> System w a) -> System w ()
sliceForMC_ (Slice vec) sys = do
  s :: Storage c <- getStore
  U.forM_ vec $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

-- | Slice version of mapM_
{-# INLINE sliceMapM_ #-}
sliceMapM_ :: Monad m => (Entity c -> m a) -> Slice c -> m ()
sliceMapM_ ma (Slice vec) = U.mapM_ (ma . Entity) vec

-- | Slice version of mapM
{-# INLINE sliceMapM #-}
sliceMapM :: Monad m => (Entity c -> m a) -> Slice c -> m [a]
sliceMapM ma (Slice vec) = traverse (ma . Entity) (U.toList vec)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE sliceMapMC #-}
sliceMapMC :: forall w c a. (Store (Storage c), Has w c) => ((Entity c,Safe c) -> System w a) -> Slice c -> System w [a]
sliceMapMC sys (Slice vec) = do
  s :: Storage c <- getStore
  for (U.toList vec) $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

-- | Iterates over a slice, and reads the components of the Slice's type argument.
{-# INLINE sliceMapMC_ #-}
sliceMapMC_ :: forall w c a. (Store (Storage c), Has w c) => ((Entity c, Safe c) -> System w a) -> Slice c -> System w ()
sliceMapMC_ sys vec = sliceForMC_ vec sys

