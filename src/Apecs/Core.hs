{-# LANGUAGE ConstraintKinds            #-}
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

-- | An Entity is just an integer, used to index into a component store.
--   In general, use @newEntity@, @cmap@, and component tags instead of manipulating these directly.
--   
--   For performance reasons, negative values like (-1) are reserved for stores to represent special values, so avoid using these.
newtype Entity = Entity {unEntity :: Int} deriving (Num, Eq, Ord, Show)

-- | A System is a newtype around `ReaderT w IO a`, where `w` is the game world variable.
--   Systems mainly serve to
--   
--   * Lift side effects into the IO Monad.
--
--   * Allow type-based lookup of a component's store through @getStore@.
newtype System w a = System {unSystem :: ReaderT w IO a} deriving (Functor, Monad, Applicative, MonadIO)

-- | A component is defined by specifying how it is stored.
--   The constraint ensures that stores and components are mapped one-to-one.
class (Elem (Storage c) ~ c) => Component c where
  type Storage c

-- | @Has w c@ means that world @w@ can produce a @Storage c@.
class Component c => Has w c where
  getStore :: System w (Storage c)

-- | The type of components stored by a store, e.g. @Elem (Map c) = c@.
type family Elem s

-- | Indicates that the store @s@ can be initialized.
--   Generally, \"base\" stores like @Map c@ can be initialized, but composite stores like @MaybeStore s@ cannot.
class ExplInit s where
  -- | Initialize a new empty store.
  explInit :: IO s

-- | Stores that we can read using @explGet@ and @explExists@.
--   For some entity @e@, @eplGet s e@ is only guaranteed to be safe if @explExists s e@ returns @True@.
class ExplGet s where
  -- | Reads a component from the store. What happens if the component does not exist is left undefined, and might not necessarily crash.
  explGet :: s -> Int -> IO (Elem s)
  -- | Returns whether there is a component for the given index.
  explExists :: s -> Int -> IO Bool

-- | Stores that can be written.
class ExplSet s where
  -- | Writes a component to the store.
  explSet :: s -> Int -> Elem s -> IO ()

-- | Stores that components can be removed from.
class ExplDestroy s where
  -- | Destroys the component for a given index.
  explDestroy :: s -> Int -> IO ()

-- | Stores that we can request a list of member entities for.
class ExplMembers s where
  -- | Returns an unboxed vector of member indices
  explMembers :: s -> IO (U.Vector Int)

type Get     w c = (Has w c, ExplGet     (Storage c))
type Set     w c = (Has w c, ExplSet     (Storage c))
type Members w c = (Has w c, ExplMembers (Storage c))
type Destroy w c = (Has w c, ExplDestroy (Storage c))

-- | Identity component/store. @Identity c@ is equivalent to @c@, so using it is mostly useless.
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

T.makeInstances [2..8]

-- | Psuedocomponent indicating the absence of @a@.
--   Mainly used as e.g. @cmap $ \(a, Not b) -> c@ to iterate over entities with an @a@ but no @b@.
--   Can also be used to delete components, like @cmap $ \a -> (Not :: Not a)@ to delete every @a@ component.
data Not a = Not

-- | Pseudostore used to produce values of type @Not a@, inverts @explExists@, and destroys instead of @explSet@.
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

-- | Pseudostore used to produce values of type @Maybe a@.
--   Will always return @True@ for @explExists@.
--   Writing can both set and delete a component using @Just@ and @Nothing@ respectively.
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

-- | Used for 'Either', a logical disjunction between two components.
--   As expected, Either is used to model error values.
-- Getting an @Either a b@ will first attempt to get a @b@ and return it as @Right b@, or if it does not exist, get an @a@ as @Left a@.
-- Can also be used to set one of two things.
data EitherStore sa sb = EitherStore sa sb
instance (Component ca, Component cb) => Component (Either ca cb) where
  type Storage (Either ca cb) = EitherStore (Storage ca) (Storage cb)

instance (Has w ca, Has w cb) => Has w (Either ca cb) where
  getStore = EitherStore <$> getStore <*> getStore

type instance Elem (EitherStore sa sb) = Either (Elem sa) (Elem sb)

instance (ExplGet sa, ExplGet sb) => ExplGet (EitherStore sa sb) where
  explGet (EitherStore sa sb) ety = do
    e <- explExists sb ety
    if e then Right <$> explGet sb ety
         else Left <$> explGet sa ety
  explExists (EitherStore sa sb) ety = do
    e <- explExists sb ety
    if e then return True
         else explExists sa ety

instance (ExplSet sa, ExplSet sb) => ExplSet (EitherStore sa sb) where
  explSet (EitherStore _ sb) ety (Right b) = explSet sb ety b
  explSet (EitherStore sa _) ety (Left a)  = explSet sa ety a

instance Has w () where
  getStore = return ()
instance Component () where
  type Storage () = ()
type instance Elem () = ()
instance ExplGet () where
  explExists _ _ = return True
  explGet _ _ = return ()
instance ExplSet () where
  explSet _ _ _ = return ()
instance ExplDestroy () where
  explDestroy _ _ = return ()

-- | Pseudocomponent that functions normally for @explExists@ and @explMembers@, but always return @Filter@ for @explGet@.
--   Can be used in cmap as @cmap $ \(Filter :: Filter a) -> b@.
--   Since the above can be written more consicely as @cmap $ \(_ :: a) -> b@, it is rarely directly.
--   More interestingly, we can define reusable filters like @movables = Filter :: Filter (Position, Velocity)@.
data Filter c = Filter deriving (Eq, Show)

-- Pseudostore for 'Filter'.
newtype FilterStore s = FilterStore s

instance Component c => Component (Filter c) where
  type Storage (Filter c) = FilterStore (Storage c)

instance Has w c => Has w (Filter c) where
  getStore = FilterStore <$> getStore

type instance Elem (FilterStore s) = Filter (Elem s)

instance ExplGet s => ExplGet (FilterStore s) where
  explGet _ _ = return Filter
  explExists (FilterStore s) ety = explExists s ety

instance ExplMembers s => ExplMembers (FilterStore s) where
  explMembers (FilterStore s) = explMembers s

-- | Pseudostore used to produce components of type 'Entity'.
-- Always returns @True@ for @explExists@, and echoes back the entity argument for @explGet@.
-- Used in e.g. @cmap $ \(a, ety :: Entity) -> b@ to access the current entity.
data EntityStore = EntityStore
instance Component Entity where
  type Storage Entity = EntityStore

instance (Has w Entity) where
  getStore = return EntityStore

type instance Elem EntityStore = Entity
instance ExplGet EntityStore where
  explGet _ ety = return $ Entity ety
  explExists _ _ = return True
