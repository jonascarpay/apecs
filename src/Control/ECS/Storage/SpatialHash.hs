{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module Control.ECS.Storage.SpatialHash (
  Indexable,
  IndexTable,
  indexSlice,
) where

import Data.Vector.Mutable as V
import qualified Data.IntSet as S
import Control.Monad.IO.Class

import Control.ECS.Core

class Indexable c where
  index :: c -> Int

data IndexTable c = IndexTable
  { table :: IOVector S.IntSet
  , main  :: c
  }

indexSlice :: forall w c. (Indexable c, Storage c ~ IndexTable c, Has w c) => c -> System w (Slice c)
indexSlice c =
  do Store (IndexTable tab _) :: Store c <- getStore
     indexSet <- liftIO $ V.read tab (index c)
     return . fromList . S.toList $ indexSet

instance ( SStorage sc
         , Bounded  (SElem sc)
         , Indexable (SElem sc)
         , SSafeElem sc ~ Maybe (SElem sc)
         ) => SStorage (IndexTable sc) where
  type SSafeElem (IndexTable sc) = SSafeElem sc
  type SElem (IndexTable sc) = SElem sc

  sEmpty = do
    let lo = index (minBound :: SElem sc)
        hi = index (maxBound :: SElem sc)
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
                       V.modify tab (S.delete e) (index x)

  sRead (IndexTable _ sc) e = sRead sc e
  sReadUnsafe (IndexTable _ sc) e = sReadUnsafe sc e

  sWrite itab Nothing e = sDestroy itab e
  sWrite (IndexTable tab sc) (Just x) e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !xOld -> V.modify tab (S.delete e) (index xOld)

       V.modify tab (S.insert e) (index x)
       sWrite sc (Just x) e

  sWriteUnsafe (IndexTable tab sc) x e =
    do mx <- sRead sc e
       case mx of
         Nothing -> return ()
         Just !xOld -> V.modify tab (S.delete e) (index xOld)

       V.modify tab (S.insert e) (index x)
       sWriteUnsafe sc x e

  {-# INLINE sEmpty #-}
  {-# INLINE sWrite #-}
  {-# INLINE sReadUnsafe #-}
  {-# INLINE sWriteUnsafe #-}
  {-# INLINE sAll #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRead #-}
