{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

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
newtype System a = System { runSystem :: IO a } deriving (Functor, Applicative, Monad)

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

-- (,)
instance (Component a, Component b) => Component (a, b) where
  type Repr    (a, b) = (Repr a, Repr b)
  type Storage (a, b) = (Storage a, Storage b)

  {-# INLINE empty #-}
  empty =
    let Store sta :: Store a = empty
        Store stb :: Store b = empty
     in Store (sta, stb)

  {-# INLINE slice #-}
  slice (Store (sta, stb)) =
    do Slice sla :: Slice a <- slice (Store sta)
       Slice slb :: Slice b <- slice (Store stb)
       return (Slice (sla `S.intersection` slb))

  {-# INLINE retrieve #-}
  retrieve ety (Store (sta, stb)) =
    do Reads ra :: Reads a <- retrieve ety (Store sta)
       Reads rb :: Reads b <- retrieve ety (Store stb)
       return (Reads (ra, rb))

  {-# INLINE store #-}
  store ety (Writes (wa, wb)) (Store (sta, stb)) =
    do Store sta' :: Store a <- store ety (Writes wa) (Store sta)
       Store stb' :: Store b <- store ety (Writes wb) (Store stb)
       return (Store (sta', stb'))

instance (w `Stores` c1, w `Stores` c2) => w `Stores` (c1, c2) where
  {-# INLINE getStore #-}
  getStore w =
    let Store s1 :: Store c1 = getStore w
        Store s2 :: Store c2 = getStore w
     in Store (s1, s2)

  {-# INLINE putStore #-}
  putStore (Store (s1, s2)) w =
    do putStore (Store s1 :: Store c1) w
       putStore (Store s2 :: Store c2) w

-- (,,)
instance (Component a, Component b, Component c) => Component (a, b, c) where
  type Repr    (a, b, c) = (Repr a, Repr b, Repr c)
  type Storage (a, b, c) = (Storage a, Storage b, Storage c)

  {-# INLINE empty #-}
  empty =
    let Store sta :: Store a = empty
        Store stb :: Store b = empty
        Store stc :: Store c = empty
     in Store (sta, stb, stc)

  {-# INLINE slice #-}
  slice (Store (sta, stb, stc)) =
    do Slice sla :: Slice a <- slice (Store sta)
       Slice slb :: Slice b <- slice (Store stb)
       Slice slc :: Slice c <- slice (Store stc)
       return (Slice (sla `S.intersection` slb `S.intersection` slc))

  {-# INLINE retrieve #-}
  retrieve ety (Store (sta, stb, stc)) =
    do Reads ra :: Reads a <- retrieve ety (Store sta)
       Reads rb :: Reads b <- retrieve ety (Store stb)
       Reads rc :: Reads c <- retrieve ety (Store stc)
       return (Reads (ra, rb, rc))

  {-# INLINE store #-}
  store ety (Writes (wa, wb, wc)) (Store (sta, stb, stc)) =
    do Store sta' :: Store a <- store ety (Writes wa) (Store sta)
       Store stb' :: Store b <- store ety (Writes wb) (Store stb)
       Store stc' :: Store c <- store ety (Writes wc) (Store stc)
       return (Store (sta', stb', stc'))

instance (w `Stores` a, w `Stores` b, w `Stores` c) => w `Stores` (a, b, c) where
  {-# INLINE getStore #-}
  getStore w =
    let Store sta :: Store a = getStore w
        Store stb :: Store b = getStore w
        Store stc :: Store c = getStore w
     in Store (sta, stb, stc)

  {-# INLINE putStore #-}
  putStore (Store (sta, stb, stc)) w =
    do putStore (Store sta :: Store a) w
       putStore (Store stb :: Store b) w
       putStore (Store stc :: Store c) w
