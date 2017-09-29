{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Apecs.Concurrent (
  concurrently,
  pcmap, prmap, pwmap, pcmap', prmap', pwmap',
) where

import qualified Control.Concurrent.Async as A
import Control.Monad.Reader
import qualified Data.Vector.Unboxed as U

import Apecs.Types
import Apecs.System

-- | Executes a list of systems concurrently, and blocks until all have finished.
--   Provides zero protection against race conditions and other hazards, so use with caution.
concurrently :: [System w ()] -> System w ()
concurrently ss = do w <- System ask
                     liftIO . A.mapConcurrently_ (runWith w) $ ss

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

-- | @cmap'@ version of @pcmap@
{-# INLINE pcmap' #-}
pcmap' :: forall world c. Has world c => Int -> (c -> Safe c) -> System world ()
pcmap' grainSize f = do
  s :: Storage c <- getStore
  liftIO$ do sl <- explMembers s
             parallelize grainSize (\e -> explGetUnsafe s e >>= explSetMaybe s e . getSafe . f) sl

-- | @rmap'@ version of @pcmap@
{-# INLINE prmap' #-}
prmap' :: forall world r w. (Has world w, Has world r, Store (Storage r), Store (Storage w))
      => Int -> (r -> Safe w) -> System world ()
prmap' grainSize f = do
  sr :: Storage r <- getStore
  sw :: Storage w <- getStore
  liftIO$ do sl <- explMembers sr
             parallelize grainSize (\e -> explGetUnsafe sr e >>= explSetMaybe sw e . getSafe . f) sl

-- | @wmap@ version of @pcmap@
{-# INLINE pwmap #-}
pwmap :: forall world r w. (Has world w, Has world r, Store (Storage r), Store (Storage w))
     => Int -> (Safe r -> w) -> System world ()
pwmap grainSize f = do
  sr :: Storage r <- getStore
  sw :: Storage w <- getStore
  liftIO$ do sl <- explMembers sr
             parallelize grainSize (\e -> explGet sr e >>= explSet sw e . f . Safe) sl

-- | @wmap'@ version of @pcmap@
{-# INLINE pwmap' #-}
pwmap' :: forall world r w. (Has world w, Has world r, Store (Storage r), Store (Storage w))
       => Int -> (Safe r -> Safe w) -> System world ()
pwmap' grainSize f =
  do sr :: Storage r <- getStore
     sw :: Storage w <- getStore
     liftIO$ do sl <- explMembers sr
                parallelize grainSize (\e -> explGet sr e >>= explSetMaybe sw e . getSafe . f . Safe) sl
