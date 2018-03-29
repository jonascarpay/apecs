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

import qualified Apecs.THTuples        as T

-- | An Entity is really just an Int in a newtype, used to index into a component store.
newtype Entity = Entity Int deriving (Eq, Ord, Show)

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
--
--   Laws:
--
--      * For all entities in @exmplMembers s@, @explExists s ety@ must be true.
--
--      * If for some entity @explExists s ety@, @explGet s ety@ should safely return a non-bottom value.
class Store s where
  -- | The type of components stored by this Store
  type Elem s

  -- | Initialize the store with its initialization arguments.
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

instance Component c => Component (Identity c) where
  type Storage (Identity c) = Identity (Storage c)

instance Has w c => Has w (Identity c) where
  getStore = Identity <$> getStore

instance Store s => Store (Identity s) where
  type Elem (Identity s) = Identity (Elem s)
  initStore = error "Initializing Pseudostore"
  explGet (Identity s) e = Identity <$> explGet s e
  explSet (Identity s) e (Identity x) = explSet s e x
  explExists  (Identity s) = explExists s
  explMembers (Identity s) = explMembers s
  explDestroy (Identity s) = explDestroy s

-- Tuple Instances
T.makeInstances [2..8]

-- | Psuedocomponent indicating the absence of @a@.
data Not a = Not

-- | Pseudostore used to produce values of type @Not a@
newtype NotStore s = NotStore s

instance Component c => Component (Not c) where
  type Storage (Not c) = NotStore (Storage c)

instance (Has w c) => Has w (Not c) where
  getStore = NotStore <$> getStore

instance Store s => Store (NotStore s) where
  type Elem (NotStore s) = Not (Elem s)
  initStore = error "Initializing Pseudostore"
  explGet _ _ = return Not
  explSet (NotStore sa) ety _ = explDestroy sa ety
  explExists (NotStore sa) ety = not <$> explExists sa ety
  explMembers _ = return mempty
  explDestroy sa ety = explSet sa ety Not

-- | Pseudostore used to produce values of type @Maybe a@
newtype MaybeStore s = MaybeStore s
instance Component c => Component (Maybe c) where
  type Storage (Maybe c) = MaybeStore (Storage c)

instance (Has w c) => Has w (Maybe c) where
  getStore = MaybeStore <$> getStore

instance Store s => Store (MaybeStore s) where
  type Elem (MaybeStore s) = Maybe (Elem s)
  initStore = error "Initializing Pseudostore"
  explGet (MaybeStore sa) ety = do
    e <- explExists sa ety
    if e then Just <$> explGet sa ety
         else return Nothing
  explSet (MaybeStore sa) ety Nothing  = explDestroy sa ety
  explSet (MaybeStore sa) ety (Just x) = explSet sa ety x
  explExists _ _ = return True
  explMembers _ = return mempty
  explDestroy (MaybeStore sa) ety = explDestroy sa ety

-- | Pseudostore used to produce values of type @Either p q@
data EitherStore sp sq = EitherStore sp sq
instance (Component p, Component q) => Component (Either p q) where
  type Storage (Either p q) = EitherStore (Storage p) (Storage q)

instance (Has w p, Has w q) => Has w (Either p q) where
  getStore = EitherStore <$> getStore <*> getStore

instance (Store sp, Store sq) => Store (EitherStore sp sq) where
  type Elem (EitherStore sp sq) = Either (Elem sp) (Elem sq)
  initStore = error "Initializing Pseudostore"
  explGet (EitherStore sp sq) ety = do
    e <- explExists sp ety
    if e then Left <$> explGet sp ety
         else Right <$> explGet sq ety
  explSet (EitherStore sp _) ety (Left p)  = explSet sp ety p
  explSet (EitherStore _ sq) ety (Right q) = explSet sq ety q
  explExists (EitherStore sp sq) ety = do
    e <- explExists sp ety
    if e then return True
         else explExists sq ety
  explMembers _ = return mempty
  explDestroy _ _ = return ()

data Filter c = Filter deriving (Eq, Show)
newtype FilterStore s = FilterStore s

instance Component c => Component (Filter c) where
  type Storage (Filter c) = FilterStore (Storage c)

instance Has w c => Has w (Filter c) where
  getStore = FilterStore <$> getStore

instance Store s => Store (FilterStore s) where
  type Elem (FilterStore s) = Filter (Elem s)
  initStore = error "Initializing Pseudostore"
  explGet _ _ = return Filter
  explSet _ _ _ = return ()
  explExists (FilterStore s) ety = explExists s ety
  explMembers (FilterStore s) = explMembers s
  explDestroy _ _ = return ()

-- | Pseudostore used to produce components of type @Entity@
data EntityStore = EntityStore
instance Component Entity where
  type Storage Entity = EntityStore

instance (Has w Entity) where
  getStore = return EntityStore

instance Store EntityStore where
  type Elem EntityStore = Entity
  initStore = error "Initializing Pseudostore"
  explGet _ ety = return $ Entity ety
  explSet _ _ _ = liftIO$ putStrLn "Warning: Writing Entity is undefined"
  explExists _ _ = return True
  explMembers _ = return mempty
  explDestroy _ _ = return ()
