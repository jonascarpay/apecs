{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Apecs.Concurrent (
  concurrently,
  pmap,
) where

import qualified Control.Concurrent.Async as A
import           Control.Monad.Reader
import qualified Data.Vector.Unboxed      as U

import           Apecs.System
import           Apecs.Core

-- | Executes a list of systems concurrently, and blocks until all have finished.
--   Provides zero protection against race conditions and other hazards, so use with caution.
concurrently :: [System w ()] -> System w ()
concurrently ss = do w <- SystemT ask
                     lift . A.mapConcurrently_ (runWith w) $ ss

-- | Parallel version of @cmap@. 
{-# INLINE pmap #-}
pmap :: forall w cx cy. (Get w IO cx, Members w IO cx, Set w IO cy)
     => Int -- ^ Entities per thread
     -> (cx -> cy) -> System w ()
pmap grainSize f =
  do sr :: Storage cx <- getStore
     sw :: Storage cy <- getStore
     lift$ do
       sl <- explMembers sr
       parallelize grainSize (\e -> explGet sr e >>= explSet sw e . f) sl

  where
    parallelize grainSize sys vec
      | U.length vec <= grainSize = U.mapM_ sys vec
      | otherwise = A.mapConcurrently_ (U.mapM_ sys) vecSplits
        where
          vecSplits = go vec
          go vec
            | U.null vec = []
            | otherwise = let (h,t) = U.splitAt grainSize vec in h : go t

