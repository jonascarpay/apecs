{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Control.ECS.Storage.Mutable where

import Control.Monad
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Maybe

import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Mutable as V

import Control.ECS.Core

newtype Global c = Global {getGlobal :: IORef c}

instance Monoid c => SStorage (Global c) where
  type SElem     (Global c) = c
  type SSafeElem (Global c) = c

  sEmpty = Global <$> newIORef mempty
  sAll    _   = return UV.empty
  sMember   _ _ = return False
  sDestroy  _ _ = return ()
  sRead (Global ref) _ = readIORef ref
  sWrite    (Global ref) x _ = writeIORef ref x
  sWriteUnsafe = sWrite
  sReadUnsafe = sRead

newtype HashTable c = HashTable {getHashTable :: H.BasicHashTable Int c}

instance SStorage (HashTable c) where
  type SSafeElem (HashTable c) = Maybe c
  type SElem     (HashTable c) = c

  sEmpty = HashTable <$> H.new
  sAll    (HashTable h) = UV.fromList . fmap fst <$> H.toList h
  sMember   (HashTable h) ety = isJust <$> H.lookup h ety
  sDestroy  (HashTable h) ety = H.delete h ety
  sRead (HashTable h) ety = H.lookup h ety
  sWrite    h Nothing ety = sDestroy h ety
  sWrite    (HashTable h) (Just x) ety = H.insert h ety x

  sWriteUnsafe (HashTable h) x ety = H.insert h ety x
  sReadUnsafe (HashTable h) ety = fromJust <$> H.lookup h ety


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

instance (SSafeElem c ~ Maybe (SElem c), SStorage c) => SStorage (Cached c) where
  type SSafeElem (Cached c) = SSafeElem c
  type SElem     (Cached c) = SElem     c

  sEmpty = newCacheWith 100 sEmpty
  sAll (Cached _ t _ m) =
    do ts <- UV.filter (/= -1) <$> UV.freeze t
       ms <- sAll m
       return $! ts UV.++ ms

  sMember (Cached s t _ m) !ety =
    do !r <- U.unsafeRead t (ety `mod` s)
       if r == ety
          then return True
          else sMember m ety

  sDestroy (Cached s t _ m) !ety =
    do !r <- U.unsafeRead t (ety `mod` s)
       if r == ety
          then U.unsafeWrite t (ety `mod` s) (-1)
          else sDestroy m ety

  sRead (Cached s t c m) !ety =
    do let !index = mod ety s
       !r <- U.unsafeRead t index
       if r == ety
          then Just <$> V.unsafeRead c index
          else sRead m ety

  sWrite c Nothing  ety = sDestroy c ety
  sWrite c (Just w) ety = sWriteUnsafe c w ety

  sWriteUnsafe (Cached s t c m) !w !ety =
    do let !index = mod ety s
       !r <- U.unsafeRead t index
       when (r /= -1 && r /= ety) $
         do !cached <- V.read c index
            sWrite m (Just cached) ety
       U.unsafeWrite t index ety
       V.unsafeWrite c index w

  sReadUnsafe (Cached s t c m) !ety =
    do let !index = mod ety s
       !r <- U.read t index
       if r == ety
          then V.unsafeRead c index
          else sReadUnsafe m ety

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}
