{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
newtype Entity = Entity {unEntity :: Int} deriving (Num, Eq, Ord, Show, Enum)

-- | A SystemT is a newtype around `ReaderT w IO a`, where `w` is the game world variable.
--   Systems mainly serve to
--
--   * Lift side effects into the IO Monad.
--
--   * Allow type-based lookup of a component's store through @getStore@.
newtype SystemT w m a = SystemT {unSystem :: ReaderT w m a} deriving (Functor, Monad, Applicative, MonadTrans, MonadIO)
type System w a = SystemT w IO a

deriving instance Monad m => MonadReader w (SystemT w m)

-- | A component is defined by specifying how it is stored.
--   The constraint ensures that stores and components are mapped one-to-one.
class (Elem (Storage c) ~ c) => Component c where
  type Storage c

-- | @Has w m c@ means that world @w@ can produce a @Storage c@.
class (Monad m, Component c) => Has w m c where
  getStore :: SystemT w m (Storage c)

-- | The type of components stored by a store, e.g. @Elem (Map c) = c@.
type family Elem s

-- | Indicates that the store @s@ can be initialized.
--   Generally, \"base\" stores like @Map c@ can be initialized, but composite stores like @MaybeStore s@ cannot.
class ExplInit m s where
  -- | Initialize a new empty store.
  explInit :: m s

-- | Stores that we can read using @explGet@ and @explExists@.
--   For some entity @e@, @eplGet s e@ is only guaranteed to be safe if @explExists s e@ returns @True@.
class Monad m => ExplGet m s where
  -- | Reads a component from the store. What happens if the component does not exist is left undefined, and might not necessarily crash.
  explGet :: s -> Int -> m (Elem s)
  -- | Returns whether there is a component for the given index.
  explExists :: s -> Int -> m Bool

-- | Stores that can be written.
class Monad m => ExplSet m s where
  -- | Writes a component to the store.
  explSet :: s -> Int -> Elem s -> m ()

-- | Stores that components can be removed from.
class Monad m => ExplDestroy m s where
  -- | Destroys the component for a given index.
  explDestroy :: s -> Int -> m ()

-- | Stores that we can request a list of member entities for.
class Monad m => ExplMembers m s where
  -- | Returns an unboxed vector of member indices
  explMembers :: s -> m (U.Vector Int)

type Get     w m c = (Has w m c, ExplGet     m (Storage c))
type Set     w m c = (Has w m c, ExplSet     m (Storage c))
type Members w m c = (Has w m c, ExplMembers m (Storage c))
type Destroy w m c = (Has w m c, ExplDestroy m (Storage c))

-- | Identity component/store. @Identity c@ is equivalent to @c@, so using it is mostly useless.
instance Component c => Component (Identity c) where
  type Storage (Identity c) = Identity (Storage c)

instance Has w m c => Has w m (Identity c) where
  {-# INLINE getStore #-}
  getStore = Identity <$> getStore

type instance Elem (Identity s) = Identity (Elem s)

instance ExplGet m s => ExplGet m (Identity s) where
  {-# INLINE explGet #-}
  explGet (Identity s) e = Identity <$> explGet s e
  {-# INLINE explExists  #-}
  explExists  (Identity s) = explExists s

instance ExplSet m s => ExplSet m (Identity s) where
  {-# INLINE explSet #-}
  explSet (Identity s) e (Identity x) = explSet s e x
instance ExplMembers m s => ExplMembers m (Identity s) where
  {-# INLINE explMembers #-}
  explMembers (Identity s) = explMembers s
instance ExplDestroy m s => ExplDestroy m (Identity s) where
  {-# INLINE explDestroy #-}
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

instance (Has w m c) => Has w m (Not c) where
  {-# INLINE getStore #-}
  getStore = NotStore <$> getStore

type instance Elem (NotStore s) = Not (Elem s)

instance ExplGet m s => ExplGet m (NotStore s) where
  {-# INLINE explGet #-}
  explGet _ _ = return Not
  {-# INLINE explExists #-}
  explExists (NotStore sa) ety = not <$> explExists sa ety

instance ExplDestroy m s => ExplSet m (NotStore s) where
  {-# INLINE explSet #-}
  explSet (NotStore sa) ety _ = explDestroy sa ety

-- | Pseudostore used to produce values of type @Maybe a@.
--   Will always return @True@ for @explExists@.
--   Writing can both set and delete a component using @Just@ and @Nothing@ respectively.
newtype MaybeStore s = MaybeStore s
instance Component c => Component (Maybe c) where
  type Storage (Maybe c) = MaybeStore (Storage c)

instance (Has w m c) => Has w m (Maybe c) where
  {-# INLINE getStore #-}
  getStore = MaybeStore <$> getStore

type instance Elem (MaybeStore s) = Maybe (Elem s)

instance ExplGet m s => ExplGet m (MaybeStore s) where
  {-# INLINE explGet #-}
  explGet (MaybeStore sa) ety = do
    e <- explExists sa ety
    if e then Just <$> explGet sa ety
         else return Nothing
  explExists _ _ = return True

instance (ExplDestroy m s, ExplSet m s) => ExplSet m (MaybeStore s) where
  {-# INLINE explSet #-}
  explSet (MaybeStore sa) ety Nothing  = explDestroy sa ety
  explSet (MaybeStore sa) ety (Just x) = explSet sa ety x

-- | Used for 'Either', a logical disjunction between two components.
--   As expected, Either is used to model error values.
-- Getting an @Either a b@ will first attempt to get a @b@ and return it as @Right b@, or if it does not exist, get an @a@ as @Left a@.
-- Can also be used to set one of two things.
data EitherStore sa sb = EitherStore sa sb
instance (Component ca, Component cb) => Component (Either ca cb) where
  type Storage (Either ca cb) = EitherStore (Storage ca) (Storage cb)

instance (Has w m ca, Has w m cb) => Has w m (Either ca cb) where
  {-# INLINE getStore #-}
  getStore = EitherStore <$> getStore <*> getStore

type instance Elem (EitherStore sa sb) = Either (Elem sa) (Elem sb)

instance (ExplGet m sa, ExplGet m sb) => ExplGet m (EitherStore sa sb) where
  {-# INLINE explGet #-}
  explGet (EitherStore sa sb) ety = do
    e <- explExists sb ety
    if e then Right <$> explGet sb ety
         else Left <$> explGet sa ety
  {-# INLINE explExists #-}
  explExists (EitherStore sa sb) ety = do
    e <- explExists sb ety
    if e then return True
         else explExists sa ety

instance (ExplSet m sa, ExplSet m sb) => ExplSet m (EitherStore sa sb) where
  {-# INLINE explSet #-}
  explSet (EitherStore _ sb) ety (Right b) = explSet sb ety b
  explSet (EitherStore sa _) ety (Left a)  = explSet sa ety a

instance (ExplDestroy m sa, ExplDestroy m sb)
       => ExplDestroy m (EitherStore sa sb) where
  {-# INLINE explDestroy #-}
  explDestroy (EitherStore sa sb) ety =
    explDestroy sa ety >> explDestroy sb ety

instance Monad m => Has w m () where
  {-# INLINE getStore #-}
  getStore = return ()
instance Component () where
  type Storage () = ()
type instance Elem () = ()
instance Monad m => ExplGet m () where
  {-# INLINE explExists #-}
  explExists _ _ = return True
  {-# INLINE explGet #-}
  explGet _ _ = return ()
instance Monad m => ExplSet m () where
  {-# INLINE explSet #-}
  explSet _ _ _ = return ()
instance Monad m => ExplDestroy m () where
  {-# INLINE explDestroy #-}
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

instance Has w m c => Has w m (Filter c) where
  {-# INLINE getStore #-}
  getStore = FilterStore <$> getStore

type instance Elem (FilterStore s) = Filter (Elem s)

instance ExplGet m s => ExplGet m (FilterStore s) where
  {-# INLINE explGet #-}
  explGet _ _ = return Filter
  {-# INLINE explExists #-}
  explExists (FilterStore s) ety = explExists s ety

instance ExplMembers m s => ExplMembers m (FilterStore s) where
  {-# INLINE explMembers #-}
  explMembers (FilterStore s) = explMembers s

-- | Pseudostore used to produce components of type 'Entity'.
-- Always returns @True@ for @explExists@, and echoes back the entity argument for @explGet@.
-- Used in e.g. @cmap $ \(a, ety :: Entity) -> b@ to access the current entity.
data EntityStore = EntityStore
instance Component Entity where
  type Storage Entity = EntityStore

instance Monad m => Has w m Entity where
  {-# INLINE getStore #-}
  getStore = return EntityStore

type instance Elem EntityStore = Entity
instance Monad m => ExplGet m EntityStore where
  {-# INLINE explGet #-}
  explGet _ ety = return $ Entity ety
  {-# INLINE explExists #-}
  explExists _ _ = return True
