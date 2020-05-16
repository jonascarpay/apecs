{-|
  Stability : experimental

  This module is experimental, and its API might change between point releases. Use at your own risk.

  Adds the @'Reactive' r s@ store, which wraps around store @s@.
  Every time a component is added, removed, or changed, 'Reactive' will call the function 'react', from the 'Reacts' type class.

  A simple example is @Reactive (Printer c) (Map c)@, which will print a message every time a component @c@ is changed.

  More importantly, there are three sets of reverse maps: maps that allow you to look up /entities/ by /component/, instead of the other way around.
  The difference between the three is their performance and the restrictions they place on their component type.

-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Experimental.Reactive
  ( Reacts (..), Reactive
  , EnumMap, enumLookup
  , OrdMap, ordLookup
  , IxMap, ixLookup
  , Printer
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Array.IO        as A
import qualified Data.IntMap.Strict   as IM
import qualified Data.IntSet          as S
import           Data.IORef
import           Data.Ix
import qualified Data.Map.Strict      as M

import           Apecs.Components
import           Apecs.Core

-- | Class required by @Reactive@.
--   Given some @r@ and update information about some component, will run a side-effect in monad @m@.
--   Note that there are also instances for @(,)@.
class Monad m => Reacts m s where
  rempty :: m s
  react  :: s -> Entity -> Maybe (Elem s) -> Maybe (Elem s) -> m ()

-- | Constructs an implementation for 'reacts' from a deletion function and insertion function.
--   @Nothing -> Nothing@ is @noop@, and @Just old -> Just new@ is @cons (delete old) (insert new)@, unless @old == new@.
--
--   The first two arguments should form a 'Monoid'.
{-# INLINE defaultReacts #-}
defaultReacts :: Eq c => r -> (r -> r -> r) -> (c -> r) -> (c -> r) -> Maybe c -> Maybe c -> r
defaultReacts noop cons delete insert = go where
  go Nothing Nothing = noop
  go (Just old) (Just new) = if old == new then noop else cons (delete old) (insert new)
  go (Just old) Nothing = delete old
  go Nothing (Just new) = insert new

-- | Wrapper for reactivity around some store s.
data Reactive r s = Reactive
  { _rtoken :: r
  , rstore :: s
  }

type instance Elem (Reactive r s) = Elem s

instance (Reacts m r, ExplInit m s) => ExplInit m (Reactive r s) where
  explInit = liftM2 Reactive rempty explInit

instance ExplGet m s => ExplGet m (Reactive r s) where
  {-# INLINE explExists #-}
  {-# INLINE explGet    #-}
  explExists = explExists . rstore
  explGet = explGet . rstore

instance (Reacts m r, ExplSet m s, ExplGet m s, Elem s ~ Elem r)
  => ExplSet m (Reactive r s) where
  {-# INLINE explSet #-}
  explSet (Reactive r s) ety c = do
    old <- explGet (MaybeStore s) ety
    react r (Entity ety) old (Just c)
    explSet s ety c

instance (Reacts m r, ExplDestroy m s, ExplGet m s, Elem s ~ Elem r)
  => ExplDestroy m (Reactive r s) where
  {-# INLINE explDestroy #-}
  explDestroy (Reactive r s) ety = do
    old <- explGet (MaybeStore s) ety
    react r (Entity ety) old Nothing
    explDestroy s ety

instance ExplMembers m s => ExplMembers m (Reactive r s) where
  {-# INLINE explMembers #-}
  explMembers (Reactive _ s) = explMembers s

-- | Prints a message to stdout every time a component is updated.
data Printer c = Printer
type instance Elem (Printer c) = c

instance (MonadIO m, Show c) => Reacts m (Printer c) where
  {-# INLINE rempty #-}
  rempty = return Printer
  {-# INLINE react #-}
  react _ (Entity ety) (Just c) Nothing = liftIO$
    putStrLn $ "Entity " ++ show ety ++ ": destroyed component " ++ show c
  react _ (Entity ety) Nothing (Just c) = liftIO$
    putStrLn $ "Entity " ++ show ety ++ ": created component " ++ show c
  react _ (Entity ety) (Just old) (Just new) = liftIO$
    putStrLn $ "Entity " ++ show ety ++ ": update component " ++ show old ++ " to " ++ show new
  react _ _ _ _ = return ()

-- | Allows you to look up entities by component value.
--   Use e.g. @withReactive $ mapLookup True@ to retrieve a list of entities that have a @True@ component.
--   Based on an @IntMap IntSet@ internally.
newtype EnumMap c = EnumMap (IORef (IM.IntMap S.IntSet))

type instance Elem (EnumMap c) = c
instance (MonadIO m, Eq c, Enum c) => Reacts m (EnumMap c) where
  {-# INLINE rempty #-}
  rempty = liftIO$ EnumMap <$> newIORef mempty
  {-# INLINE react #-}
  react (EnumMap ref) (Entity ety) old new = liftIO . modifyIORef' ref $
    defaultReacts id (flip (.)) delete insert old new
      where
        delete c = IM.adjust (S.delete ety) (fromEnum c)
        insert c = IM.insertWith mappend (fromEnum c) (S.singleton ety)

{-# INLINE enumLookup #-}
enumLookup
  :: forall w m c q.
     ( Storage c ~ Reactive (EnumMap c) q
     , Has w m c
     , MonadIO m
     , Enum c)
  => c -> SystemT w m [Entity]
enumLookup c = do
  Reactive (EnumMap ref) _ :: Storage c <- getStore
  emap <- liftIO $ readIORef ref
  return $ maybe [] (fmap Entity . S.toList) (IM.lookup (fromEnum c) emap)

-- | Allows you to look up entities by component value.
--   Based on a @Map c IntSet@ internally
newtype OrdMap c = OrdMap (IORef (M.Map c S.IntSet))

type instance Elem (OrdMap c) = c
instance (MonadIO m, Ord c) => Reacts m (OrdMap c) where
  {-# INLINE rempty #-}
  rempty = liftIO$ OrdMap <$> newIORef mempty
  {-# INLINE react #-}
  react (OrdMap ref) (Entity ety) old new = liftIO . modifyIORef' ref $
    defaultReacts id (flip (.)) delete insert old new
      where
        delete = M.adjust (S.delete ety)
        insert c = M.insertWith mappend c (S.singleton ety)

{-# INLINE ordLookup #-}
ordLookup
  :: forall w m c q.
     ( Storage c ~ Reactive (OrdMap c) q
     , Has w m c
     , MonadIO m
     , Ord c)
  => c -> SystemT w m [Entity]
ordLookup c = do
  Reactive (OrdMap ref) _ :: Storage c <- getStore
  omap <- liftIO $ readIORef ref
  return $ maybe [] (fmap Entity . S.toList) (M.lookup c omap)

-- | Allows you to look up entities by component value.
--   Based on an @IOArray c IntSet@ internally
newtype IxMap c = IxMap (A.IOArray c S.IntSet)

{-# INLINE modifyArray #-}
modifyArray :: Ix i => A.IOArray i a -> i -> (a -> a) -> IO ()
modifyArray ref ix f = A.readArray ref ix >>= A.writeArray ref ix . f

type instance Elem (IxMap c) = c
instance (MonadIO m, Ix c, Bounded c) => Reacts m (IxMap c) where
  {-# INLINE rempty #-}
  rempty = liftIO$ IxMap <$> A.newArray (minBound, maxBound) mempty
  {-# INLINE react #-}
  react (IxMap ref) (Entity ety) old new = liftIO $
    defaultReacts (pure ()) (>>) delete insert old new where
      delete c = modifyArray ref c (S.delete ety)
      insert c = modifyArray ref c (S.insert ety)

{-# INLINE ixLookup #-}
ixLookup :: (MonadIO m, Ix c) => c -> IxMap c -> m [Entity]
ixLookup c = go where
  go (IxMap ref) = liftIO $ fmap Entity . S.toList <$> A.readArray ref c
