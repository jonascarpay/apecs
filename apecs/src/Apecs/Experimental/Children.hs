{-|
Stability: experimental

This module is experimental, and its API might change between point releases.
Use at your own risk.

The default relation between an entity and a component value is one to zero
or one. The entity may or may not have a value for the component, but if the
component value exists, it belongs to an entity. This module enables setting
multiple "child" component values rooted under the same "parent" entity,
providing a one to many relation: the parent entity has zero or more child
values of the component type. Concretely, these component values are of type
'Child' @c@, belong to their own separate entities, and are explicitly linked
to the parent entity.

Ad-hoc child relationships may be established without using this module by
including a parent 'Entity' in your component's type, but this is limiting in
regards to traversing the relationship. Systems concerned with the relationship
may only start from the child entities' component(s) and then fetch the parent
entity's component(s). By expressing the relationship using this module, you get
support for iteration over the parent-child relationship in whichever way is
more convenient for your systems, i.e. you can map over child entities using the
'Child' component then fetch the child entity's parent component(s) as needed,
or you can map over the parent entities' 'ChildList' component then fetch the
child entities' component(s) as needed.

Some example use cases for this module:

- Parent entity has a position defined in world space and child entities have
data relative to the parent's position e.g. hitboxes, sprite animations, etc.
- Parent entity is a leader and child entities are squad members e.g. a
necromancer can summon skeletons

For an introduction to using this module, see the [associated
example](https://github.com/jonascarpay/apecs/tree/master/examples/Children.hs).
-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Apecs.Experimental.Children
  ( -- * Component
    Child(..)
    -- * Pseudocomponents
  , ChildValue(..)
  , ChildList(..)
  ) where

import Apecs.Core
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Type.Reflection (TypeRep, Typeable, typeRep)

import qualified Data.IORef as IORef
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Unboxed as U

-- | The 'Child' component wraps the parent entity and the child entity's
-- underlying component value.
--
-- If you want a @Foo@ component in your game to be treated as a child
-- component, specify the component type as @Child Foo@ when declaring your
-- world:
--
-- > newtype Hitbox = Hitbox AABB deriving Show
-- > instance Component Hitbox where type Storage Hitbox = Map Hitbox
-- >
-- > -- A type alias solely for TH quoting's sake.
-- > type ChildHitbox = Child Hitbox
-- >
-- > makeWorld "World" [''ChildHitbox]
--
-- If your system is iterating over the 'Child' component but does not need the
-- parent entity, use the 'ChildValue' pseudocomponent instead for better
-- performance.
--
-- Note that if you delete a parent entity (i.e. 'Apecs.System.destroy'
-- all of the parent entity's components), consider a
-- 'Apecs.System.destroy' on the parent entity's children too. See
-- 'ChildList' for assistance on this. This is more from a memory
-- management point of view than one of safety: nothing via standard
-- usage of this library will break if a child "outlives" its
-- parent. However, both trying to directly 'Apecs.System.get' some
-- component value of a child's non-existent parent or trying to
-- directly 'Apecs.System.get' a parent's non-existent 'ChildList' will
-- result in runtime errors. Raw use of 'Apecs.System.get' is inherently
-- dangerous and its risk is not specific to the behavior provided by
-- this module.
data Child c = Child !Entity !c deriving (Eq, Show)
instance Component c => Component (Child c) where
  type Storage (Child c) = Children (Storage c)

-- | 'Children' augments another store with support for one-to-many parent-child
-- relationships.
--
-- This wrapper is not exported. If the user wants a @Foo@ component to be
-- treated as a child component, they declare their component when building
-- their world as type @Child Foo@. This will cause the @Children@ store wrapper
-- to be used via the @Storage@/@Elem@ type relation.
data Children s = Children
  { childrenParentToChildren :: !(IORef (IntMap IntSet))
  , childrenChildToParent :: !(IORef (IntMap Int))
  , childrenDelegate :: !s
  }
type instance Elem (Children s) = Child (Elem s)

instance (MonadIO m, ExplInit m s) => ExplInit m (Children s) where
  {-# INLINE explInit #-}
  explInit :: m (Children s)
  explInit = do
    childrenDelegate <- explInit
    liftIO $ do
      childrenParentToChildren <- IORef.newIORef M.empty
      childrenChildToParent <- IORef.newIORef M.empty
      pure Children
        { childrenParentToChildren
        , childrenChildToParent
        , childrenDelegate
        }

instance (MonadIO m, ExplMembers m s) => ExplMembers m (Children s) where
  {-# INLINE explMembers #-}
  explMembers :: Children s -> m (U.Vector Int)
  explMembers (Children _ _ s) = explMembers s

instance (MonadIO m, ExplGet m s, Typeable (Elem s)) => ExplGet m (Children s) where
  {-# INLINE explGet #-}
  explGet :: Children s -> Int -> m (Child (Elem s))
  explGet (Children _ childToParent s) child = do
    liftIO (M.lookup child <$> IORef.readIORef childToParent) >>= \case
      Nothing -> error $ parentNotFound (typeRep @(Elem s)) child
      Just parent -> do
        component <- explGet s child
        pure $ Child (Entity parent) component

  {-# INLINE explExists #-}
  explExists :: Children s -> Int -> m Bool
  explExists (Children _ _ s) = explExists s

instance (MonadIO m, ExplSet m s) => ExplSet m (Children s) where
  {-# INLINE explSet #-}
  explSet :: Children s -> Int -> Child (Elem s) -> m ()
  explSet (Children parentToChildren childToParent s) child (Child (Entity parent) x) = do
    explSet s child x
    liftIO $ do
      (mPrevParent, childToParentMap') <-
        M.insertLookupWithKey insertChildToParent child parent
          <$> IORef.readIORef childToParent
      -- @insertLookupWithKey@ uses a @StrictPair@ internally for its result
      -- before converting to standard pair, so there's no need to evaluate
      -- @childToParentMap'@ here before writing it to the @IORef@.
      IORef.writeIORef childToParent childToParentMap'
      IORef.modifyIORef' parentToChildren
        $ M.insertWith S.union parent (S.singleton child)
        . case mPrevParent of
            -- If the child was previously mapped to a different parent, be sure
            -- to clean up the old mapping from parent to child.
            Just prevParent | prevParent /= parent ->
              M.update (deleteParentToChild child) prevParent
            _ -> id
    where
    insertChildToParent :: M.Key -> Int -> Int -> Int
    insertChildToParent _k newParent _prevParent = newParent

instance (MonadIO m, ExplDestroy m s) => ExplDestroy m (Children s) where
  {-# INLINE explDestroy #-}
  explDestroy :: Children s -> Int -> m ()
  explDestroy (Children parentToChildren childToParent s) child = do
    explDestroy s child
    liftIO $ do
      childToParentMap <- IORef.readIORef childToParent
      case M.updateLookupWithKey deleteChildToParent child childToParentMap of
        (Nothing, _) -> do
          -- If the parent entity can't be found, assume the child was
          -- previously destroyed.
          pure ()
        (Just parent, childToParentMap') -> do
          -- @updateLookupWithKey@ uses a @StrictPair@ internally for its result
          -- before converting to standard pair, so there's no need to evaluate
          -- @childToParentMap'@ here before writing it to the @IORef@.
          IORef.writeIORef childToParent childToParentMap'
          IORef.modifyIORef' parentToChildren
            $ M.update (deleteParentToChild child) parent
    where
    deleteChildToParent :: M.Key -> Int -> Maybe Int
    deleteChildToParent _k _v = Nothing

-- | Accessor pseudocomponent that produces just the underlying component value
-- as opposed to 'Child' which also produces the parent entity.
--
-- For best performance, you should prefer 'ChildValue' over 'Child' if your
-- system is iterating over children and does not need the parent entities.
newtype ChildValue c = ChildValue c deriving (Eq, Show)
instance Component c => Component (ChildValue c) where
  type Storage (ChildValue c) = ChildValueStore (Storage c)

newtype ChildValueStore s = ChildValueStore (Children s)
type instance Elem (ChildValueStore s) = ChildValue (Elem s)

instance (MonadIO m, Component c, Has w m (Child c)) => Has w m (ChildValue c) where
  {-# INLINE getStore #-}
  getStore :: SystemT w m (Storage (ChildValue c))
  getStore = ChildValueStore <$> getStore

instance ExplMembers m s => ExplMembers m (ChildValueStore s) where
  {-# INLINE explMembers #-}
  explMembers :: ChildValueStore s -> m (U.Vector Int)
  explMembers (ChildValueStore (Children _ _ s)) = explMembers s

instance ExplGet m s => ExplGet m (ChildValueStore s) where
  {-# INLINE explExists #-}
  explExists :: ChildValueStore s -> Int -> m Bool
  explExists (ChildValueStore (Children _ _ s)) = explExists s

  {-# INLINE explGet #-}
  explGet :: ChildValueStore s -> Int -> m (ChildValue (Elem s))
  explGet (ChildValueStore (Children _ _ s)) child =
    ChildValue <$> explGet s child

-- | Pseudocomponent that produces all child entities for a parent.
--
-- A useful property of this pseudocomponent is that it may be destroyed, which
-- does a cascading 'Apecs.System.destroy' on all of the parent's children:
--
-- > -- Remove all of player 1 entity's hitboxes:
-- > destroy player1 $ Proxy @(ChildList Hitbox)
--
-- The cascading 'Apecs.System.destroy' behavior is provided for convenience,
-- but note that if you assigned additional components to the child entities,
-- those components will not be destroyed. In this case, you should destroy
-- all components on the children explicitly, e.g.:
--
-- > ChildList children :: ChildList Hitbox <- get player1
-- > for_ children $ \child -> do
-- >   destroy child $ Proxy @ComponentsToDestroy
newtype ChildList c = ChildList (NonEmpty Entity) deriving (Eq, Show)
instance Component c => Component (ChildList c) where
  type Storage (ChildList c) = ChildListStore (Storage c)

newtype ChildListStore s = ChildListStore (Children s)
type instance Elem (ChildListStore s) = ChildList (Elem s)

instance (MonadIO m, Component c, Has w m (Child c)) => Has w m (ChildList c) where
  {-# INLINE getStore #-}
  getStore :: SystemT w m (Storage (ChildList c))
  getStore = ChildListStore <$> getStore

instance MonadIO m => ExplMembers m (ChildListStore s) where
  {-# INLINE explMembers #-}
  explMembers :: ChildListStore s -> m (U.Vector Int)
  explMembers (ChildListStore (Children parentToChildren _ _)) = do
    liftIO $ U.fromList . M.keys <$> IORef.readIORef parentToChildren

instance (MonadIO m, Typeable (Elem s)) => ExplGet m (ChildListStore s) where
  {-# INLINE explExists #-}
  explExists :: ChildListStore s -> Int -> m Bool
  explExists (ChildListStore (Children parentToChildren _ _)) parent = do
    liftIO $ M.member parent <$> IORef.readIORef parentToChildren

  {-# INLINE explGet #-}
  explGet :: ChildListStore s -> Int -> m (ChildList (Elem s))
  explGet (ChildListStore (Children parentToChildren _ _)) parent = do
    liftIO (toNE . M.lookup parent <$> IORef.readIORef parentToChildren) >>= \case
      Nothing -> error $ parentNotFound (typeRep @(Elem s)) parent
      Just children -> pure $ ChildList children
    where
    toNE :: Maybe IntSet -> Maybe (NonEmpty Entity)
    toNE mChildEnts
      | Just childEnts <- mChildEnts = NE.nonEmpty (Entity <$> S.elems childEnts)
      | otherwise = Nothing

instance (MonadIO m, ExplDestroy m s) => ExplDestroy m (ChildListStore s) where
  {-# INLINE explDestroy #-}
  explDestroy :: ChildListStore s -> Int -> m ()
  explDestroy (ChildListStore children@(Children parentToChildren _ _)) parent = do
    liftIO (M.lookup parent <$> IORef.readIORef parentToChildren) >>= \case
      Nothing -> pure ()
      Just childSet -> do
        for_ (S.elems childSet) $ \child -> do
          explDestroy children child

deleteParentToChild :: Int -> IntSet -> Maybe IntSet
deleteParentToChild child v
  | v' <- S.delete child v, not $ S.null v' = Just v'
  | otherwise = Nothing

parentNotFound :: TypeRep a -> Int -> String
parentNotFound tyRep ety =
  unwords
    [ "Reading non-existent parent entity for child component of type"
    , show tyRep
    , "for child entity"
    , show ety
    ]
