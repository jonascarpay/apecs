{-# LANGUAGE DataKinds, BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.ECS.Storage.Mutable
  ( Global, Cache,
  ) where

import Control.Monad
import GHC.TypeLits
import Data.Proxy
import Data.IORef

import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Mutable         as V

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

data Cache (n :: Nat) s = Cache
  { size  :: Int
  , tags  :: U.IOVector Int
  , cache :: V.IOVector (SElem s)
  , main  :: s
  }

instance (KnownNat n, SSafeElem c ~ Maybe (SElem c), SStorage c) => SStorage (Cache n c) where
  type SSafeElem (Cache n c) = SSafeElem c
  type SElem     (Cache n c) = SElem     c

  sEmpty =
    do let size = fromInteger$ natVal (Proxy :: Proxy n)
       tags  <- U.replicate size (-1)
       cache <- V.new size
       main  <- sEmpty
       return (Cache size tags cache main)

  sAll (Cache _ t _ m) =
    do ts <- UV.filter (/= -1) <$> UV.freeze t
       ms <- sAll m
       return $! ts UV.++ ms

  sMember (Cache s t _ m) !ety =
    do !r <- U.unsafeRead t (ety `mod` s)
       if r == ety
          then return True
          else sMember m ety

  sDestroy (Cache s t _ m) !ety =
    do !r <- U.unsafeRead t (ety `mod` s)
       if r == ety
          then U.unsafeWrite t (ety `mod` s) (-1)
          else sDestroy m ety

  sRead (Cache s t c m) !ety =
    do let !index = mod ety s
       !r <- U.unsafeRead t index
       if r == ety
          then Just <$> V.unsafeRead c index
          else sRead m ety

  sWrite c Nothing  ety = sDestroy c ety
  sWrite c (Just w) ety = sWriteUnsafe c w ety

  sWriteUnsafe (Cache s t c m) !w !ety =
    do let !index = mod ety s
       !r <- U.unsafeRead t index
       when (r /= -1 && r /= ety) $
         do !cached <- V.read c index
            sWrite m (Just cached) ety
       U.unsafeWrite t index ety
       V.unsafeWrite c index w

  sReadUnsafe (Cache s t c m) !ety =
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
