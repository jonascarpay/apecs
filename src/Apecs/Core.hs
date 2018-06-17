{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Core where

import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Vector.Unboxed   as U

-- import qualified Apecs.THTuples        as T

-- | An Entity is just an integer, used to index into a component store.
newtype Entity = Entity {unEntity :: Int} deriving (Num, Eq, Ord, Show)

-- | A System is a newtype around `ReaderT w IO a`, where `w` is the game world variable.
newtype System w a = System {unSystem :: ReaderT w IO a} deriving (Functor, Monad, Applicative, MonadIO)

-- | A component is defined by the type of its storage
--   The storage in turn supplies runtime types for the component.
--   For the component to be valid, its Storage must be an instance of Store.
class (Elem (Storage c) ~ c) => Component c where
  type Storage c

-- | A world `Has` a component if it can produce its Storage
class Component c => Has w c where
  getStore :: System w (Storage c)

-- | The type of components stored by a Store
type family Elem s

-- | Holds components indexed by entities
class ExplInit s where
  -- | Initialize the store with its initialization arguments.
  explInit :: IO s

-- | Stores that support @get@ and @exists@ in the IO monad
--   If @existsIO@
class ExplGet s where
  -- | Reads a component from the store. What happens if the component does not exist is left undefined.
  explGet :: s -> Int -> IO (Elem s)
  -- | Returns whether there is a component for the given index
  explExists :: s -> Int -> IO Bool

class ExplSet s where
  -- | Writes a component
  explSet :: s -> Int -> Elem s -> IO ()

class ExplDestroy s where
  -- | Destroys the component for a given index.
  explDestroy :: s -> Int -> IO ()

class ExplMembers s where
  -- | Returns an unboxed vector of member indices
  explMembers :: s -> IO (U.Vector Int)

instance Component c => Component (Identity c) where
  type Storage (Identity c) = Identity (Storage c)

instance Has w c => Has w (Identity c) where
  getStore = Identity <$> getStore

type instance Elem (Identity s) = Identity (Elem s)

instance ExplGet s => ExplGet (Identity s) where
  explGet (Identity s) e = Identity <$> explGet s e
  explExists  (Identity s) = explExists s

instance ExplSet s => ExplSet (Identity s) where
  explSet (Identity s) e (Identity x) = explSet s e x
instance ExplMembers s => ExplMembers (Identity s) where
  explMembers (Identity s) = explMembers s
instance ExplDestroy s => ExplDestroy (Identity s) where
  explDestroy (Identity s) = explDestroy s

-- Tuple Instances TODO
-- T.makeInstances [2..8]

-- | Psuedocomponent indicating the absence of @a@.
data Not a = Not

-- | Pseudostore used to produce values of type @Not a@
newtype NotStore s = NotStore s

instance Component c => Component (Not c) where
  type Storage (Not c) = NotStore (Storage c)

instance (Has w c) => Has w (Not c) where
  getStore = NotStore <$> getStore

type instance Elem (NotStore s) = Not (Elem s)

instance ExplGet s => ExplGet (NotStore s) where
  explGet _ _ = return Not
  explExists (NotStore sa) ety = not <$> explExists sa ety

instance ExplDestroy s => ExplSet (NotStore s) where
  explSet (NotStore sa) ety _ = explDestroy sa ety

-- | Pseudostore used to produce values of type @Maybe a@
newtype MaybeStore s = MaybeStore s
instance Component c => Component (Maybe c) where
  type Storage (Maybe c) = MaybeStore (Storage c)

instance (Has w c) => Has w (Maybe c) where
  getStore = MaybeStore <$> getStore

type instance Elem (MaybeStore s) = Maybe (Elem s)

instance ExplGet s => ExplGet (MaybeStore s) where
  explGet (MaybeStore sa) ety = do
    e <- explExists sa ety
    if e then Just <$> explGet sa ety
         else return Nothing
  explExists _ _ = return True

instance (ExplDestroy s, ExplSet s) => ExplSet (MaybeStore s) where
  explSet (MaybeStore sa) ety Nothing  = explDestroy sa ety
  explSet (MaybeStore sa) ety (Just x) = explSet sa ety x

data Filter c = Filter deriving (Eq, Show)
newtype FilterStore s = FilterStore s

instance Component c => Component (Filter c) where
  type Storage (Filter c) = FilterStore (Storage c)

instance Has w c => Has w (Filter c) where
  getStore = FilterStore <$> getStore

type instance Elem (FilterStore s) = Filter (Elem s)

instance ExplGet s => ExplGet (FilterStore s) where
  explGet _ _ = return Filter
  explExists (FilterStore s) ety = explExists s ety

-- | Pseudostore used to produce components of type @Entity@
data EntityStore = EntityStore
instance Component Entity where
  type Storage Entity = EntityStore

instance (Has w Entity) where
  getStore = return EntityStore

type instance Elem EntityStore = Entity
instance ExplGet EntityStore where
  explGet _ ety = return $ Entity ety
  explExists _ _ = return True
