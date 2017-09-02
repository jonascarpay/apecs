{-# LANGUAGE UndecidableInstances, DataKinds, BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.ECS.Storage.Mutable
  ( Global, readGlobal, writeGlobal,
    Cache,
    EnumTable, indexSlice,

  ) where

import Control.Monad
import Control.Monad.IO.Class
import GHC.TypeLits
import Data.Proxy
import Data.IORef
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Mutable         as V

import Control.ECS.Core as ECS

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

{-# INLINE readGlobal #-}
readGlobal :: Has w c => System w (Reads c)
readGlobal = ECS.read 0

{-# INLINE writeGlobal #-}
writeGlobal :: Has w c => Writes c -> System w ()
writeGlobal c = write c 0

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

data EnumTable c = EnumTable (V.IOVector S.IntSet) c

indexSlice :: forall w c a. (Enum c, Storage c ~ EnumTable a, Has w c) => c -> System w (Slice c)
indexSlice c =
  do Store (EnumTable tab _) :: Store c <- getStore
     indexSet <- liftIO $ V.read tab (fromEnum c)
     return . sliceFromList . S.toList $ indexSet

instance ( SStorage sc
         , Bounded  (SElem sc)
         , Enum (SElem sc)
         , SSafeElem sc ~ Maybe (SElem sc)
         ) => SStorage (EnumTable sc) where
  type SSafeElem (EnumTable sc) = SSafeElem sc
  type SElem (EnumTable sc) = SElem sc

  sEmpty = do
    let lo = fromEnum (minBound :: SElem sc)
        hi = fromEnum (maxBound :: SElem sc)
        s  = hi - lo + 1
    tab <- V.replicate s mempty
    sc <- sEmpty
    return $ EnumTable tab sc

  sAll (EnumTable _ sc) = sAll sc
  sMember (EnumTable _ sc) = sMember sc

  sDestroy (EnumTable tab sc) e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !x -> do sDestroy sc e
                       V.modify tab (S.delete e) (fromEnum x)

  sRead (EnumTable _ sc) e = sRead sc e
  sReadUnsafe (EnumTable _ sc) e = sReadUnsafe sc e

  sWrite itab Nothing e = sDestroy itab e
  sWrite (EnumTable tab sc) (Just x) e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !xOld -> V.modify tab (S.delete e) (fromEnum xOld)

       V.modify tab (S.insert e) (fromEnum x)
       sWrite sc (Just x) e

  sWriteUnsafe (EnumTable tab sc) x e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !xOld -> V.modify tab (S.delete e) (fromEnum xOld)

       V.modify tab (S.insert e) (fromEnum x)
       sWriteUnsafe sc x e

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}
