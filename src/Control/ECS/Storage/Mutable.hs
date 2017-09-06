{-# LANGUAGE UndecidableInstances, DataKinds, BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.ECS.Storage.Mutable
  ( Global, readGlobal, writeGlobal,
    Cache,
    IndexTable, indexSlice,

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
readGlobal :: forall w c. (Storage c ~ Global c, Has w c) => System w c
readGlobal = do Store (Global ref) :: Store c <- getStore
                liftIO$ readIORef ref

{-# INLINE writeGlobal #-}
writeGlobal :: forall w c. (Storage c ~ Global c, Has w c) => c -> System w ()
writeGlobal c = do Store (Global ref) :: Store c <- getStore
                   liftIO$ writeIORef ref c

{-# INLINE modifyGlobal #-}
modifyGlobal :: forall w c. (Storage c ~ Global c, Has w c) => (c -> c) -> System w ()
modifyGlobal f = do Store (Global ref) :: Store c <- getStore
                    liftIO$ modifyIORef' ref f

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

class Indexable c where
  toIndex :: c -> Int

data IndexTable c = IndexTable (V.IOVector S.IntSet) c

indexSlice :: forall w c a. (Indexable c, Storage c ~ IndexTable a, Has w c) => c -> System w (Slice c)
indexSlice c =
  do Store (IndexTable tab _) :: Store c <- getStore
     indexSet <- liftIO $ V.read tab (toIndex c)
     return . sliceFromList . S.toList $ indexSet

instance ( SStorage sc
         , Bounded  (SElem sc)
         , Indexable (SElem sc)
         , SSafeElem sc ~ Maybe (SElem sc)
         ) => SStorage (IndexTable sc) where
  type SSafeElem (IndexTable sc) = SSafeElem sc
  type SElem (IndexTable sc) = SElem sc

  sEmpty = do
    let lo = toIndex (minBound :: SElem sc)
        hi = toIndex (maxBound :: SElem sc)
        s  = hi - lo + 1
    tab <- V.replicate s mempty
    sc <- sEmpty
    return $ IndexTable tab sc

  sAll (IndexTable _ sc) = sAll sc
  sMember (IndexTable _ sc) = sMember sc

  sDestroy (IndexTable tab sc) e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !x -> do sDestroy sc e
                       V.modify tab (S.delete e) (toIndex x)

  sRead (IndexTable _ sc) e = sRead sc e
  sReadUnsafe (IndexTable _ sc) e = sReadUnsafe sc e

  sWrite itab Nothing e = sDestroy itab e
  sWrite (IndexTable tab sc) (Just x) e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !xOld -> V.modify tab (S.delete e) (toIndex xOld)

       V.modify tab (S.insert e) (toIndex x)
       sWrite sc (Just x) e

  sWriteUnsafe (IndexTable tab sc) x e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !xOld -> V.modify tab (S.delete e) (toIndex xOld)

       V.modify tab (S.insert e) (toIndex x)
       sWriteUnsafe sc x e

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}
