
module Control.ECS.Storage.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.IORef
import Control.Monad
import Data.Vector.Unboxed as U

import Control.ECS.Storage

newtype Map c = Map {unMap :: IORef (M.IntMap c)}

instance SStorage IO (Map c) where
  type SSafeElem (Map c) = Maybe c
  type SElem     (Map c) = c

  sEmpty = Map <$> newIORef mempty
  sSlice    (Map m) = U.fromList . M.keys <$> readIORef m
  sMember   (Map m) ety = M.member ety <$> readIORef m
  sDestroy  (Map m) ety = modifyIORef' m (M.delete ety)
  sRetrieve (Map m) ety = M.lookup ety <$> readIORef m
  sStore    m Nothing ety = sDestroy m ety
  sStore    (Map m) (Just x) ety = modifyIORef' m (M.insert ety x)

  sWUnsafe (Map m) x ety = modifyIORef' m (M.insert ety x)
  sRUnsafe (Map m) ety = do mx <- M.lookup ety <$> readIORef m
                            case mx of
                              Nothing -> error "Unsafe read miss"
                              Just x -> return x

newtype FlagSet = FlagSet {unFlagSet :: IORef S.IntSet}

instance SStorage IO FlagSet where
  type SSafeElem FlagSet = Bool
  type SElem     FlagSet = Bool

  sEmpty = FlagSet <$> newIORef mempty
  sSlice (FlagSet s) = U.fromList . S.toList <$> readIORef s
  sMember (FlagSet s) ety = S.member ety <$> readIORef s
  sDestroy (FlagSet s) ety = modifyIORef' s (S.delete ety)
  sRetrieve = sMember
  sStore s False ety = sDestroy s ety
  sStore (FlagSet s) True ety = modifyIORef' s (S.insert ety)

  sWUnsafe = sStore
  sRUnsafe = sRetrieve

