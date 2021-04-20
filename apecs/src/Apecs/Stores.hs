{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Stores
  ( Map, Cache, Unique,
    Global,
    Cachable,
    ReadOnly, setReadOnly, destroyReadOnly
    -- Register, regLookup
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Bits                   (shiftL, (.&.))
import qualified Data.IntMap.Strict          as M
import           Data.Proxy
import           Data.Typeable               (Typeable, typeRep)
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits

import           Apecs.Core

-- | A map based on 'Data.IntMap.Strict'. O(log(n)) for most operations.
newtype Map c = Map (M.IntMap c)

type instance Elem (Map c) = c
instance Applicative m => ExplInit m (Map c) where
  explInit = pure (Map mempty)

instance (Monad m, Typeable c) => ExplGet m (Map c) where
  explExists (Map ref) ety = return (M.member ety ref)
  explGet    (Map ref) ety = return $ case M.lookup ety ref of
    Just c -> c
    notFound -> error $ unwords
      [ "Reading non-existent Map component"
      , show (typeRep notFound)
      , "for entity"
      , show ety
      ]
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance Monad m => ExplSet m (Map c) where
  {-# INLINE explSet #-}
  explSet (Map ref) ety x = return $ Map $ M.insert ety x ref

instance Monad m => ExplDestroy m (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map ref) ety = return $ Map $ M.delete ety ref

instance Monad m => ExplMembers m (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map ref) = return $ U.fromList $ M.keys ref

-- | A Unique contains zero or one component.
--   Writing to it overwrites both the previous component and its owner.
--   Its main purpose is to be a 'Map' optimized for when only ever one component inhabits it.
newtype Unique c = Unique (Maybe (Int, c))
type instance Elem (Unique c) = c
instance Monad m => ExplInit m (Unique c) where
  explInit = pure (Unique Nothing)

instance (Monad m, Typeable c) => ExplGet m (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = return $ case ref of
    Just (_, c)  -> c
    notFound -> error $ unwords
      [ "Reading non-existent Unique component"
      , show (typeRep notFound)
      ]

  {-# INLINE explExists #-}
  explExists (Unique ref) ety = return $ maybe False ((==ety) . fst) ref

instance Monad m => ExplSet m (Unique c) where
  {-# INLINE explSet #-}
  explSet _ ety c = return $ Unique $ Just (ety, c)

instance Monad m => ExplDestroy m (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy u@(Unique ref) ety = return $ case ref of
    Just (ety', _) | ety' == ety -> Unique Nothing
    _ -> u

instance Monad m => ExplMembers m (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = return $ case ref of
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

-- | A 'Global' contains exactly one component.
--   The initial value is 'mempty' from the component's 'Monoid' instance.
--   Querying a 'Global' at /any/ Entity yields this one component, effectively sharing the component between /all/ entities.
--
--   A Global component can be read with @'get' 0@ or @'get' 1@ or even @'get' undefined@.
--   The convenience entity 'global' is defined as -1, and can be used to make operations on a global more explicit, i.e. 'Time t <- get global'.
--
--   You also can read and write Globals during a 'cmap' over other components.
newtype Global c = Global c
type instance Elem (Global c) = c
instance (Monoid c, Monad m) => ExplInit m (Global c) where
  {-# INLINE explInit #-}
  explInit = return $ Global mempty

instance Monad m => ExplGet m (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = return ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance Monad m => ExplSet m (Global c) where
  {-# INLINE explSet #-}
  explSet _ _ c = return $ Global c

-- | Class of stores that behave like a regular map, and can therefore safely be cached.
--   This prevents stores like `Unique` and 'Global', which do /not/ behave like simple maps, from being cached.
class Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

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
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Elem s)) s

cacheMiss :: t
cacheMiss = error "Cache miss! If you are seeing this during normal operation, please open a bug report at https://github.com/jonascarpay/apecs"

type instance Elem (Cache n s) = Elem s

instance (MonadIO m, ExplInit m s, KnownNat n, Cachable s) => ExplInit m (Cache n s) where
  {-# INLINE explInit #-}
  explInit = do
    let n = fromIntegral$ natVal (Proxy @n) :: Int
        size = head . dropWhile (<n) $ iterate (`shiftL` 1) 1
        mask = size - 1
    tags <- liftIO$ UM.replicate size (-2)
    cache <- liftIO$ VM.replicate size cacheMiss
    child <- explInit
    return (Cache mask tags cache child)

instance (MonadIO m, ExplGet m s) => ExplGet m (Cache n s) where
  {-# INLINE explGet #-}
  explGet (Cache mask tags cache s) ety = do
    let index = ety .&. mask
    tag <- liftIO$ UM.unsafeRead tags index
    if tag == ety
       then liftIO$ VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explExists #-}
  explExists (Cache mask tags _ s) ety = do
    tag <- liftIO$ UM.unsafeRead tags (ety .&. mask)
    if tag == ety then return True else explExists s ety

instance (MonadIO m, ExplSet m s) => ExplSet m (Cache n s) where
  {-# INLINE explSet #-}
  explSet c@(Cache mask tags cache s) ety x = do
    let index = ety .&. mask
    tag <- liftIO$ UM.unsafeRead tags index
    c' <- if (tag /= (-2) && tag /= ety)
      then do
        cached <- liftIO$ VM.unsafeRead cache index
        s' <- explSet s tag cached
        return $ Cache mask tags cache s'
      else return c
    liftIO$ UM.unsafeWrite tags  index ety
    liftIO$ VM.unsafeWrite cache index x
    return c'

instance (MonadIO m, ExplDestroy m s) => ExplDestroy m (Cache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (Cache mask tags cache s) ety = do
    let index = ety .&. mask
    tag <- liftIO$ UM.unsafeRead tags (ety .&. mask)
    when (tag == ety) $ liftIO $ do
      UM.unsafeWrite tags  index (-2)
      VM.unsafeWrite cache index cacheMiss
    s' <- explDestroy s ety
    return (Cache mask tags cache s')

instance (MonadIO m, ExplMembers m s) => ExplMembers m (Cache n s) where
  {-# INLINE explMembers #-}
  explMembers (Cache mask tags _ s) = do
    cached <- liftIO$ U.filter (/= (-2)) <$> U.freeze tags
    let etyFilter ety = (/= ety) <$> UM.unsafeRead tags (ety .&. mask)
    stored <- explMembers s >>= liftIO . U.filterM etyFilter
    return $! cached U.++ stored

-- | Wrapper that makes a store read-only by hiding its 'ExplSet' and 'ExplDestroy' instances.
--   This is primarily used to protect the 'EntityCounter' from accidental overwrites.
--   Use 'setReadOnly' and 'destroyReadOnly' to override.
newtype ReadOnly s = ReadOnly s
type instance Elem (ReadOnly s) = Elem s

instance (Functor m, ExplInit m s) => ExplInit m (ReadOnly s) where
  explInit = ReadOnly <$> explInit

instance ExplGet m s => ExplGet m (ReadOnly s) where
  explExists (ReadOnly s) = explExists s
  explGet    (ReadOnly s) = explGet s
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplMembers m s => ExplMembers m (ReadOnly s) where
  {-# INLINE explMembers #-}
  explMembers (ReadOnly s) = explMembers s

setReadOnly :: forall w m s c.
  ( Has w m c
  , Storage c ~ ReadOnly s
  , Elem s ~ c
  , ExplSet m s
  ) => Entity -> c -> SystemT w m ()
setReadOnly (Entity ety) c = do
  ReadOnly s :: ReadOnly s <- getStore
  s' <- lift $ explSet s ety c
  setStore $ ReadOnly s'

destroyReadOnly :: forall w m s c.
  ( Has w m c
  , Storage c ~ ReadOnly s
  , Elem s ~ c
  , ExplDestroy m s
  ) => Entity -> Proxy c -> SystemT w m ()
destroyReadOnly (Entity ety) _ = do
  ReadOnly s :: Storage c <- getStore
  s' <- lift $ explDestroy s ety
  setStore $ ReadOnly s'
