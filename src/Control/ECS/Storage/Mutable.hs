{-# LANGUAGE FlexibleContexts #-}

module Control.ECS.Storage.Mutable where

import Control.Monad
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Maybe

import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Mutable as V

import Control.ECS.Storage

newtype Global c = Global {getGlobal :: IORef c}

instance Monoid c => SStorage IO (Global c) where
  type SElem     (Global c) = c
  type SSafeElem (Global c) = c

  sEmpty = Global <$> newIORef mempty
  sSlice    _   = return []
  sMember   _ _ = return False
  sDestroy  _ _ = return ()
  sRetrieve (Global ref) _ = readIORef ref
  sStore    (Global ref) x _ = writeIORef ref x
  sOver     (Global ref) = modifyIORef' ref
  sForC     (Global ref) f = void $ readIORef ref >>= f

  {-# INLINE sEmpty #-}
  {-# INLINE sStore #-}
  {-# INLINE sOver #-}
  {-# INLINE sForC #-}
  {-# INLINE sSlice #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRetrieve #-}


newtype HashTable c = HashTable {getHashTable :: H.BasicHashTable Int c}

instance SStorage IO (HashTable c) where
  type SSafeElem (HashTable c) = Maybe c
  type SElem     (HashTable c) = c

  sEmpty = HashTable <$> H.new
  sSlice    (HashTable h) = fmap (Entity . fst) <$> H.toList h
  sMember   (HashTable h) (Entity ety) = isJust <$> H.lookup h ety
  sDestroy  (HashTable h) (Entity ety) = H.delete h ety
  sRetrieve (HashTable h) (Entity ety) = H.lookup h ety
  sStore    h Nothing ety = sDestroy h ety
  sStore    (HashTable h) (Just x) (Entity ety) = H.insert h ety x
  sOver     (HashTable h) f = flip H.mapM_ h $ \(k,x) -> H.insert h k (f x)
  sForC     (HashTable h) fm = flip H.mapM_ h $ \(_,x) -> fm x


data Cached s = Cached { size  :: Int
                       , tags  :: U.IOVector Int
                       , cache :: V.IOVector (SElem s)
                       , main  :: s
                       }

newCacheWith :: (SSafeElem s) ~ Maybe (SElem s) => Int -> IO s -> IO (Cached s)
newCacheWith cacheSize sub =
  do t <- U.replicate cacheSize (-1)
     c <- V.new cacheSize
     m <- sub
     return (Cached cacheSize t c m)

instance (SSafeElem c ~ Maybe (SElem c), SStorage IO c) => SStorage IO (Cached c) where
  type SSafeElem (Cached c) = SSafeElem c
  type SElem     (Cached c) = SElem     c

  sEmpty = newCacheWith 100 sEmpty
  sSlice (Cached s t _ m) =
    do ts <- filter (-1==) <$> traverse (U.read t) [0..s-1]
       ms <- sSlice m
       return (fmap Entity ts ++ ms)

  sMember (Cached s t _ m) (Entity ety) =
    do r <- U.read t (ety `mod` s)
       if r == ety
          then return True
          else sMember m (Entity ety)

  sDestroy (Cached s t _ m) (Entity ety) =
    do r <- U.read t (ety `mod` s)
       if r == ety
          then U.write t (ety `mod` s) (-1)
          else sDestroy m (Entity ety)

  sRetrieve (Cached s t c m) (Entity ety) =
    do let index = mod ety s
       r <- U.read t index
       if r == ety
          then Just <$> V.read c index
          else sRetrieve m (Entity ety)

  sStore c Nothing ety = sDestroy c ety
  sStore (Cached s t c m) (Just w) (Entity ety) =
    do let index = mod ety s
       r <- U.read t index
       when (r /= -1 && r /= ety) $
         do cached <- V.read c index
            sStore m (Just cached) (Entity ety)
       U.write t index ety
       V.write c index w

  sOver (Cached s t c m) f =
    do ts <- filter (-1==) <$> traverse (U.read t) [0..s-1]
       mapM_ (V.modify c f) ts
       sOver m f


  sForC (Cached s t c m) fm =
    do ts <- filter (-1==) <$> traverse (U.read t) [0..s-1]
       mapM_ (V.read c >=> fm) ts
       sForC m fm

  {-# INLINE sEmpty #-}
  {-# INLINE sStore #-}
  {-# INLINE sOver #-}
  {-# INLINE sForC #-}
  {-# INLINE sSlice #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRetrieve #-}


