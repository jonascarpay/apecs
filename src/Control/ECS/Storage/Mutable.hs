{-# LANGUAGE FlexibleContexts #-}

module Control.ECS.Storage.Mutable where

import Control.Monad
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Maybe

import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Unboxed         as UV
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
  sWUnsafe = sStore
  sRUnsafe = sRetrieve

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

  sWUnsafe (HashTable h) x (Entity ety) = H.insert h ety x
  sRUnsafe (HashTable h) (Entity ety) = fromJust <$> H.lookup h ety


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
    do ts <- filter (-1/=) <$> traverse (U.unsafeRead t) [0..s-1]
       ms <- sSlice m
       return (fmap Entity ts ++ ms)

  sMember (Cached s t _ m) (Entity ety) =
    do r <- U.unsafeRead t (ety `mod` s)
       if r == ety
          then return True
          else sMember m (Entity ety)

  sDestroy (Cached s t _ m) (Entity ety) =
    do r <- U.unsafeRead t (ety `mod` s)
       if r == ety
          then U.write t (ety `mod` s) (-1)
          else sDestroy m (Entity ety)

  sRetrieve (Cached s t c m) (Entity ety) =
    do let index = mod ety s
       r <- U.unsafeRead t index
       if r == ety
          then Just <$> V.unsafeRead c index
          else sRetrieve m (Entity ety)

  sStore c Nothing  ety = sDestroy c ety
  sStore c (Just w) ety = sWUnsafe c w ety

  sWUnsafe (Cached s t c m) w (Entity ety) =
    do let index = mod ety s
       r <- U.unsafeRead t index
       when (r /= -1 && r /= ety) $
         do cached <- V.unsafeRead c index
            sStore m (Just cached) (Entity ety)
       U.write t index ety
       V.write c index w

  sRUnsafe (Cached s t c m) (Entity ety) =
    do let index = mod ety s
       r <- U.unsafeRead t index
       if r == ety
          then V.unsafeRead c index
          else sRUnsafe m (Entity ety)

  {-# INLINE sEmpty #-}
  {-# INLINE sStore #-}
  {-# INLINE sRUnsafe #-}
  {-# INLINE sWUnsafe #-}
  {-# INLINE sSlice #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRetrieve #-}
