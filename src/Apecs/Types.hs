{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Types where

import           Control.Monad.Reader
import qualified Data.Vector.Unboxed  as U

import qualified Apecs.THTuples       as T

-- | An Entity is really just an Int in a newtype.
newtype Entity = Entity {unEntity :: Int} deriving (Eq, Ord, Show)

-- | A slice is a list of entities, represented by a Data.Unbox.Vector of Ints.
newtype Slice = Slice {unSlice :: U.Vector Int} deriving (Show, Monoid)

-- | A system is a newtype around `ReaderT w IO a`, where `w` is the game world variable.
newtype System w a = System {unSystem :: ReaderT w IO a} deriving (Functor, Monad, Applicative, MonadIO)

-- | A component is defined by the type of its storage
--   The storage in turn supplies runtime types for the component.
--   For the component to be valid, its Storage must be an instance of Store.
class (Elem (Storage c) ~ c, Store (Storage c)) => Component c where
  type Storage c

-- | A world `Has` a component if it can produce its Storage
class Component c => Has w c where
  getStore :: System w (Storage c)

-- | Holds components indexed by entities
class Store s where
  -- | The type of components stored by this Store
  type Elem s

  -- Initialize the store with its initialization arguments.
  initStore :: IO s

  -- | Writes a component
  explSet :: s -> Int -> Elem s -> IO ()
  -- | Reads a component from the store. What happens if the component does not exist is left undefined.
  explGet :: s -> Int -> IO (Elem s)
  -- | Destroys the component for a given index.
  explDestroy :: s -> Int -> IO ()
  -- | Returns an unboxed vector of member indices
  explMembers :: s -> IO (U.Vector Int)

  -- | Returns whether there is a component for the given index
  explExists :: s -> Int -> IO Bool
  explExists s n = do
    mems <- explMembers s
    return $ U.elem n mems

  -- | Removes all components.
  --   Equivalent to calling @explDestroy@ on each member
  {-# INLINE explReset #-}
  explReset :: s -> IO ()
  explReset s = do
    sl <- explMembers s
    U.mapM_ (explDestroy s) sl

-- Tuple Instances
T.makeInstances [2..6]

-- | Psuedocomponent indicating the absence of @a@.
data Not a = Not
newtype NotStore a = NotStore (Storage a)

instance Component a => Component (Not a) where
  type Storage (Not a) = NotStore a

instance (Has w a) => Has w (Not a) where
  getStore = NotStore <$> getStore

instance Component a => Store (NotStore a) where
  type Elem (NotStore a) = Not a
  explGet _ _ = return Not
  explSet (NotStore sa) ety _ = explDestroy sa ety
  explExists (NotStore sa) ety = not <$> explExists sa ety
  explMembers _ = return mempty


newtype MaybeStore a = MaybeStore (Storage a)
instance Component a => Component (Maybe a) where
  type Storage (Maybe a) = MaybeStore a

instance (Has w a) => Has w (Maybe a) where
  getStore = MaybeStore <$> getStore

instance Component a => Store (MaybeStore a) where
  type Elem (MaybeStore a) = Maybe a
  explGet (MaybeStore sa) ety = do
    e <- explExists sa ety
    if e then Just <$> explGet sa ety
         else return Nothing
  explSet (MaybeStore sa) ety Nothing = explDestroy sa ety
  explSet (MaybeStore sa) ety (Just x) = explSet sa ety x
  explExists _ _ = return True
  explMembers _ = return mempty
