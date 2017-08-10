module Control.ECS.Types where

import qualified Data.IntSet as S

{-| An entity consists of
      1. an identification tag
      2. a collection components that hold the entity state

    Rather than storing entities, we store individual components
    along with the ID of the entity they belong to.
    This means that an entity is mostly implicit; an entity
    'exists' as long as there are components associated with
    its ID. Our world state consists mostly of component storages.
-}
newtype Entity = Entity Int

class Component comp where

  type Repr    comp :: *
  type Storage comp :: *

  empty    :: Store comp
  slice    :: Store comp -> System (Slice comp)
  retrieve :: Entity -> Store comp -> System (Reads comp)
  store    :: Entity -> Writes comp -> Store comp -> System (Store comp)

class Component comp => world `Stores` comp where
  getStore :: world -> Store comp
  putStore :: Store comp -> world -> System ()

-- | System is just a newtype around IO.
newtype System a = System (IO a) deriving (Functor, Applicative, Monad)

{-| A Slice is a collection of entities. Its type parameter c
    indicates that all contained entities have, at least
    at the time of instantiation, a component c. --}
--}
newtype Slice  comp = Slice S.IntSet

{-| A Reads c contains a runtime representation of the
    component c that was read from c's component storage
--}

newtype Reads comp = Reads (Repr comp)
{-| A Writes c contains a runtime representation of the
    component c that is to be written to c's component storage
--}
newtype Writes comp = Writes (Repr comp)

{-| A Store c contains c's global storage, as it either was
    read from or should be written to the global world state.
-}
newtype Store comp = Store (Storage comp)

