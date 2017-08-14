module Control.ECS.Storage.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.IORef
import Control.Monad

import Control.ECS.Storage

newtype Map c = Map {unMap :: IORef (M.IntMap c)}

instance SStorage IO (Map c) where
  type SSafeElem (Map c) = Maybe c
  type SElem     (Map c) = c

  sEmpty = Map <$> newIORef mempty
  sSlice    (Map m) = fmap Entity . M.keys <$> readIORef m
  sMember   (Map m) (Entity ety) = M.member ety <$> readIORef m
  sDestroy  (Map m) (Entity ety) = modifyIORef' m (M.delete ety)
  sRetrieve (Map m) (Entity ety) = M.lookup ety <$> readIORef m
  sStore    m Nothing ety = sDestroy m ety
  sStore    (Map m) (Just x) (Entity ety) = modifyIORef' m (M.insert ety x)

  sWUnsafe (Map m) x (Entity ety) = modifyIORef' m (M.insert ety x)
  sRUnsafe (Map m) (Entity ety) = do mx <- M.lookup ety <$> readIORef m
                                     case mx of
                                       Nothing -> error "Unsafe read miss"
                                       Just x -> return x

newtype FlagSet c = FlagSet {unFlagSet :: IORef S.IntSet}

instance SStorage IO (FlagSet c) where
  type SSafeElem (FlagSet c) = Bool
  type SElem     (FlagSet c) = Bool

  sEmpty = FlagSet <$> newIORef mempty
  sSlice (FlagSet s) = fmap Entity . S.toList <$> readIORef s
  sMember (FlagSet s) (Entity ety) = S.member ety <$> readIORef s
  sDestroy (FlagSet s) (Entity ety) = modifyIORef' s (S.delete ety)
  sRetrieve = sMember
  sStore s False ety = sDestroy s ety
  sStore (FlagSet s) True (Entity ety) = modifyIORef' s (S.insert ety)

  sWUnsafe = sStore
  sRUnsafe = sRetrieve

