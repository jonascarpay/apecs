{-|
Stability : experimental

This module is experimental, and its API might change between point releases. Use at your own risk.

Adds the @Reactive r s@ store, which when wrapped around store @s@, will call the @react@ on its @r@.

@Show c => Reactive (Printer c) (Map c)@ will print a message every time a @c@ value is set.

@Enum c => Reactive (EnumMap c) (Map c)@ allows you to look up entities by component value.
Use e.g. @rget >>= mapLookup True@ to retrieve a list of entities that have a @True@ component.

-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Experimental.Reactive
  ( Reacts (..), Reactive, withReactive
  , EnumMap, enumLookup
  , OrdMap, ordLookup
  , IxMap, ixLookup
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
class Monad m => Reacts m r where
  rempty :: m r
  react  :: Entity -> Maybe (Elem r) -> Maybe (Elem r) -> r -> m ()

-- | Wrapper for reactivity around some store s.
data Reactive r s = Reactive r s

type instance Elem (Reactive r s) = Elem s

-- | Performs an action with a reactive state token.
withReactive :: forall w m r s a.
             ( Component (Elem r)
             , Has w m (Elem r)
             , Storage (Elem r) ~ Reactive r s
             ) => (r -> m a) -> SystemT w m a
withReactive f = do
  Reactive r (_ :: s) <- getStore
  lift$ f r

instance (Reacts m r, ExplInit m s) => ExplInit m (Reactive r s) where
  explInit = liftM2 Reactive rempty explInit

instance (Reacts m r, ExplSet m s, ExplGet m s, Elem s ~ Elem r)
  => ExplSet m (Reactive r s) where
  {-# INLINE explSet #-}
  explSet (Reactive r s) ety c = do
    old <- explGet (MaybeStore s) ety
    react (Entity ety) old (Just c) r
    explSet s ety c

instance (Reacts m r, ExplDestroy m s, ExplGet m s, Elem s ~ Elem r)
  => ExplDestroy m (Reactive r s) where
  {-# INLINE explDestroy #-}
  explDestroy (Reactive r s) ety = do
    old <- explGet (MaybeStore s) ety
    react (Entity ety) old Nothing r
    explDestroy s ety

instance ExplGet m s => ExplGet m (Reactive r s) where
  {-# INLINE explExists #-}
  explExists (Reactive _ s) = explExists s
  {-# INLINE explGet    #-}
  explGet    (Reactive _ s) = explGet    s

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
  react (Entity ety) (Just c) Nothing _ = liftIO$
    putStrLn $ "Entity " ++ show ety ++ ": destroyed component " ++ show c
  react (Entity ety) Nothing (Just c) _ = liftIO$
    putStrLn $ "Entity " ++ show ety ++ ": created component " ++ show c
  react (Entity ety) (Just old) (Just new) _ = liftIO$
    putStrLn $ "Entity " ++ show ety ++ ": update component " ++ show old ++ " to " ++ show new
  react _ _ _ _ = return ()

-- | Allows you to look up entities by component value.
--   Use e.g. @withReactive $ mapLookup True@ to retrieve a list of entities that have a @True@ component.
--   Based on an @IntMap IntSet@ internally.
newtype EnumMap c = EnumMap (IORef (IM.IntMap S.IntSet))

type instance Elem (EnumMap c) = c
instance (MonadIO m, Enum c) => Reacts m (EnumMap c) where
  {-# INLINE rempty #-}
  rempty = liftIO$ EnumMap <$> newIORef mempty
  {-# INLINE react #-}
  react _ Nothing Nothing _ = return ()
  react (Entity ety) (Just c) Nothing (EnumMap ref) = liftIO$
    modifyIORef' ref (IM.adjust (S.delete ety) (fromEnum c))
  react (Entity ety) Nothing (Just c) (EnumMap ref) = liftIO$
    modifyIORef' ref (IM.insertWith mappend (fromEnum c) (S.singleton ety))
  react (Entity ety) (Just old) (Just new) (EnumMap ref) = liftIO$ do
    modifyIORef' ref (IM.adjust (S.delete ety) (fromEnum old))
    modifyIORef' ref (IM.insertWith mappend (fromEnum new) (S.singleton ety))

{-# INLINE enumLookup #-}
enumLookup :: (MonadIO m, Enum c) => c -> EnumMap c -> m [Entity]
enumLookup c = \(EnumMap ref) -> do
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
  react _ Nothing Nothing _ = return ()
  react (Entity ety) (Just c) Nothing (OrdMap ref) = liftIO$
    modifyIORef' ref (M.adjust (S.delete ety) c)
  react (Entity ety) Nothing (Just c) (OrdMap ref) = liftIO$
    modifyIORef' ref (M.insertWith mappend c (S.singleton ety))
  react (Entity ety) (Just old) (Just new) (OrdMap ref) = liftIO$ do
    modifyIORef' ref (M.adjust (S.delete ety) old)
    modifyIORef' ref (M.insertWith mappend new (S.singleton ety))

{-# INLINE ordLookup #-}
ordLookup :: (MonadIO m, Ord c) => c -> OrdMap c -> m [Entity]
ordLookup c = \(OrdMap ref) -> do
  emap <- liftIO $ readIORef ref
  return $ maybe [] (fmap Entity . S.toList) (M.lookup c emap)

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
  react _ Nothing Nothing _ = return ()
  react (Entity ety) (Just c) Nothing (IxMap ref) = liftIO$
    modifyArray ref c (S.delete ety)
  react (Entity ety) Nothing (Just c) (IxMap ref) = liftIO$
    modifyArray ref c (S.insert ety)
  react (Entity ety) (Just old) (Just new) (IxMap ref) = liftIO$ do
    modifyArray ref old (S.delete ety)
    modifyArray ref new (S.insert ety)

{-# INLINE ixLookup #-}
ixLookup :: (MonadIO m, Ix c) => c -> IxMap c -> m [Entity]
ixLookup c = \(IxMap ref) -> do
  liftIO $ fmap Entity . S.toList <$> A.readArray ref c
