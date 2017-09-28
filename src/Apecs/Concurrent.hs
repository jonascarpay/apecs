{-# LANGUAGE ScopedTypeVariables #-}

module Apecs.Concurrent (
  concurrently,
  pcmap, prmap
) where


import qualified Control.Concurrent.Async as A
import Control.Monad.Reader
import qualified Data.Vector.Unboxed as U

import Apecs.Types
import Apecs.System

-- | Executes a list of systems concurrently, and blocks until all have finished.
--   Provides zero protection against race conditions, so use with caution.
concurrently :: [System w ()] -> System w ()
concurrently ss = do w <- System ask
                     liftIO . A.mapConcurrently_ (runWith w) $ ss

-- | Executes a map in parallel by requesting a slice of all components,
--   and spawning threads iterating over @grainSize@ components each.
{-# INLINE pcmap #-}
pcmap :: forall world c. Has world c => Int -> (c -> c) -> System world ()
pcmap grainSize f = do
  s :: Storage c <- getStore
  liftIO$ do
    sl <- explMembers s
    parallelize grainSize (\e -> explModify s e f) sl

-- | @rmap@ version of @pcmap@
{-# INLINE prmap #-}
prmap :: forall world r w. (Has world w, Has world r)
      => Int -> (r -> w) -> System world ()
prmap grainSize f =
  do sr :: Storage r <- getStore
     sw :: Storage w <- getStore
     liftIO$ do
       sl <- explMembers sr
       parallelize grainSize (\e -> explGetUnsafe sr e >>= explSet sw e . f) sl

{-# INLINE parallelize #-}
parallelize :: U.Unbox a => Int -> (a -> IO b) -> U.Vector a -> IO ()
parallelize grainSize sys vec
  | U.length vec <= grainSize = U.mapM_ sys vec
  | otherwise = A.mapConcurrently_ (U.mapM_ sys) vecSplits
    where
      vecSplits = go vec
      go vec
        | U.null vec = []
        | otherwise = let (h,t) = U.splitAt grainSize vec in h : go t
