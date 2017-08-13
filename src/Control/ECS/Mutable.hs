{-# LANGUAGE UndecidableInstances #-}

module Control.ECS.Mutable where

import Control.Monad.State
import qualified Data.HashTable.IO as H
import qualified Data.IntSet as S
import Data.IORef

import Control.ECS.Core

class MutStorage c where
  type MutRuntime c :: *
  type Elem c :: *
  mutEmpty    :: IO c
  mutSlice    :: c -> IO [Entity]
  mutRetrieve :: Entity -> c -> IO (MutRuntime c)
  mutUnsafeRt :: Entity -> c -> IO (Elem c)
  mutStore    :: Entity -> MutRuntime c -> c -> IO ()
  mutMember   :: Entity -> c -> IO Bool
  mutDestroy  :: Entity -> c -> IO ()
  mutMapM     :: ((Entity, Elem c) -> IO a) -> c -> IO ()

{-# INLINE mutForM #-}
mutForM :: MutStorage c => c -> ((Entity, Elem c) -> IO a) -> IO ()
mutForM = flip mutMapM

instance (MutStorage a, MutStorage b) => MutStorage (a, b) where
  type MutRuntime (a, b) = (MutRuntime a, MutRuntime b)
  type Elem (a, b) = (Elem a, Elem b)
  mutEmpty = (,) <$> mutEmpty <*> mutEmpty
  mutSlice (sa, sb) = do sla <- mutSlice sa
                         filterM (`mutMember` sb) sla
  mutMember e (a, b) = (&&) <$> mutMember e a <*> mutMember e b
  mutStore e (sa, sb) (a, b) = mutStore e sa a >> mutStore e sb b
  mutRetrieve e (a, b) = (,) <$> mutRetrieve e a <*> mutRetrieve e b
  mutUnsafeRt e (a, b) = (,) <$> mutUnsafeRt e a <*> mutUnsafeRt e b
  mutDestroy e (a, b) = mutDestroy e a >> mutDestroy e b
  mutMapM f (a, b) = mutMapM sys a
    where sys (e, va) = do ee <- mutMember e b
                           when ee . void $ do vb <- mutUnsafeRt e b
                                               f (e, (va, vb))
  {-# INLINE mutEmpty #-}
  {-# INLINE mutSlice #-}
  {-# INLINE mutMember #-}
  {-# INLINE mutRetrieve #-}
  {-# INLINE mutUnsafeRt #-}
  {-# INLINE mutStore #-}
  {-# INLINE mutDestroy #-}
  {-# INLINE mutMapM #-}

data MutableMap c = MutableMap { mmKeys :: IORef S.IntSet, mmTable :: H.BasicHashTable Int c }

instance SStorage (MutableMap c) where
  type SRuntime (MutableMap c) = Maybe c
  sEmpty      = liftIO mutEmpty
  sSlice      = get >>= liftIO . mutSlice
  sRetrieve e = get >>= liftIO . mutRetrieve e
  sStore e c  = get >>= liftIO . mutStore e c
  sMember e   = get >>= liftIO . mutMember e
  sDestroy e  = get >>= liftIO . mutDestroy e

instance MutStorage (MutableMap c) where
  type MutRuntime (MutableMap c) = Maybe c
  type Elem (MutableMap c) = c

  mutEmpty = MutableMap <$> newIORef mempty <*> H.new
  mutSlice (MutableMap k _) = fmap Entity . S.toList <$> readIORef k
  mutRetrieve (Entity e) (MutableMap _ m) = H.lookup m e
  mutUnsafeRt (Entity e) (MutableMap _ m) = do Just x <- H.lookup m e
                                               return x
  mutDestroy (Entity e) (MutableMap k m)  = H.delete m e >> modifyIORef' k (S.delete e)
  mutMember (Entity e) (MutableMap k _)   = S.member e <$> readIORef k
  mutStore e Nothing m = mutDestroy e m
  mutStore (Entity e) (Just x) (MutableMap k m) = H.insert m e x >> modifyIORef' k (S.insert e)
  mutMapM f (MutableMap _ m) = H.mapM_ f' m
    where f' (e, x) = f (Entity e, x)

  {-# INLINE mutEmpty #-}
  {-# INLINE mutSlice #-}
  {-# INLINE mutMember #-}
  {-# INLINE mutRetrieve #-}
  {-# INLINE mutUnsafeRt #-}
  {-# INLINE mutStore #-}
  {-# INLINE mutDestroy #-}
  {-# INLINE mutMapM #-}
