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
  sOver     (Map m) f = modifyIORef' m (fmap f)
  sForC     (Map m) fm = do e <- readIORef m
                            mapM_ fm e


newtype FlagSet c = FlagSet {unFlagSet :: IORef S.IntSet}

instance SStorage IO (FlagSet c) where
  type SSafeElem (FlagSet c) = Bool
  type SElem     (FlagSet c) = ()

  sEmpty = FlagSet <$> newIORef mempty
  sSlice (FlagSet m) = fmap Entity . S.toList <$> readIORef m
  sMember (FlagSet m) (Entity ety) = S.member ety <$> readIORef m
  sDestroy (FlagSet m) (Entity ety) = modifyIORef' m (S.delete ety)
  sRetrieve = sMember
  sStore m False ety = sDestroy m ety
  sStore (FlagSet m) True (Entity ety) = modifyIORef' m (S.insert ety)
  sOver _ _ = return ()
  sForC (FlagSet m) fm = do s <- S.size <$> readIORef m
                            replicateM_ s (fm ())

