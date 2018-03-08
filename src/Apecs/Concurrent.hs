{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Apecs.Concurrent (
  concurrently,
  prmap,
) where

import qualified Control.Concurrent.Async as A
import           Control.Monad.Reader
import qualified Data.Vector.Unboxed      as U

import           Apecs.System
import           Apecs.Types

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

-- | @rmap@ version of @pcmap@
{-# INLINE prmap #-}
prmap :: forall world r w. (Has world w, Has world r)
      => Int -> (r -> w) -> System world ()
prmap grainSize f =
  do sr :: Storage r <- getStore
     sw :: Storage w <- getStore
     liftIO$ do
       sl <- explMembers sr
       parallelize grainSize (\e -> explGet sr e >>= explSet sw e . f) sl
