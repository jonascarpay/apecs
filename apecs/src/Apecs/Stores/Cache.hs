{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Apecs.Stores.Cache where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive     (PrimState)
import           Data.Bits                   (shiftL, (.&.))
import           Data.Proxy
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits

import           Apecs.Core

-- | Class of stores that behave like a regular map, and can therefore safely be cached.
--   This prevents stores like `Unique` and 'Global', which do /not/ behave like simple maps, from being cached.
class Cachable s
instance (KnownNat n, Cachable s) => Cachable (GCache v n s)

-- | A cache around another store.
--   Caches store their members in a fixed-size vector, so read/write operations become O(1).
--   Caches can provide huge performance boosts, especially when working with large numbers of components.
--
--   The cache size is given as a type-level argument.
--
--   Note that iterating over a cache is linear in cache size, so sparsely populated caches might /decrease/ performance.
--   In general, the exact size of the cache does not matter as long as it reasonably approximates the number of components present.
--
--   The cache uses entity (-2) internally to represent missing entities.
--   If you manually manipulate Entity values, be careful that you do not use (-2)
--
--   The actual cache is not necessarily the given argument, but the next biggest power of two.
--   This is allows most operations to be expressed as bit masks, for a large potential performance boost.
data GCache v (n :: Nat) s =
  GCache Int (UM.IOVector Int) (v (PrimState IO) (Elem s)) s

cacheMiss :: t
cacheMiss = error "Cache miss! If you are seeing this during normal operation, please open a bug report at https://github.com/jonascarpay/apecs"

type instance Elem (GCache v n s) = Elem s

type Cache n s = GCache VM.MVector n s

{-# ANN module "hlint: ignore Use <$>" #-}
instance (GM.MVector v (Elem s), MonadIO m, ExplInit m s, KnownNat n, Cachable s) => ExplInit m (GCache v n s) where
  {-# INLINE explInit #-}
  explInit = do
    let n = fromIntegral$ natVal (Proxy @n) :: Int
        size = head . dropWhile (<n) $ iterate (`shiftL` 1) 1
        mask = size - 1
    tags <- liftIO$ UM.replicate size (-2)
    cache <- liftIO$ GM.replicate size cacheMiss
    child <- explInit
    return (GCache mask tags cache child)

instance (GM.MVector v (Elem s), MonadIO m, ExplGet m s) => ExplGet m (GCache v n s) where
  {-# INLINE explGet #-}
  explGet (GCache mask tags cache child) ety = do
    let index = ety .&. mask
    tag <- liftIO$ UM.unsafeRead tags index
    if tag == ety
       then liftIO$ GM.unsafeRead cache index
       else explGet child ety

  {-# INLINE explExists #-}
  explExists (GCache mask tags _ child) ety = do
    tag <- liftIO$ UM.unsafeRead tags (ety .&. mask)
    if tag == ety then return True else explExists child ety

instance (GM.MVector v (Elem s), MonadIO m, ExplSet m s) => ExplSet m (GCache v n s) where
  {-# INLINE explSet #-}
  explSet (GCache mask tags cache child) ety x = do
    let index = ety .&. mask
    tag <- liftIO$ UM.unsafeRead tags index
    when (tag /= (-2) && tag /= ety) $ do
      cached <- liftIO$ GM.unsafeRead cache index
      explSet child tag cached
    liftIO$ UM.unsafeWrite tags  index ety
    liftIO$ GM.unsafeWrite cache index x

instance (GM.MVector v (Elem s), MonadIO m, ExplDestroy m s) => ExplDestroy m (GCache v n s) where
  {-# INLINE explDestroy #-}
  explDestroy (GCache mask tags cache child) ety = do
    let index = ety .&. mask
    tag <- liftIO$ UM.unsafeRead tags (ety .&. mask)
    when (tag == ety) $ liftIO $ do
      UM.unsafeWrite tags  index (-2)
      GM.unsafeWrite cache index cacheMiss
    explDestroy child ety

instance (GM.MVector v (Elem s), MonadIO m, ExplMembers m s) => ExplMembers m (GCache v n s) where
  {-# INLINE explMembers #-}
  explMembers (GCache mask tags _ child) = do
    cached <- liftIO$ U.filter (/= (-2)) <$> U.freeze tags
    let etyFilter ety = (/= ety) <$> UM.unsafeRead tags (ety .&. mask)
    stored <- explMembers child >>= liftIO . U.filterM etyFilter
    return $! cached U.++ stored
