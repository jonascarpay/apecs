{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Apecs.Focus
  ( Components,
    Default,
    focusField,
    focusStore,
    HasStore,
    HasField,
    GetStore,
  )
where

import Control.Monad.Reader
import Data.Kind
import GHC.Generics
import GHC.TypeLits

type family Append (l :: [Type]) (r :: [Type]) :: [Type] where
  Append (l : ls) rs = l : Append ls rs
  Append '[] rs = rs

-- It seems that finding the correct store is a lot easier if we first figure
-- out what store we're looking for, and then try to find that store.

-- Step 1
-- Figure out the store type from component type

type family Components (t :: Type) :: [Type]

type family Elem (c :: Type) (cs :: [Type]) (t :: r) (f :: r) :: r where
  Elem c '[] t f = f
  Elem c (c ': cs) t f = t
  Elem c (q ': cs) t f = Elem c cs t f

type family Unique (ts :: [t]) (e0 :: ErrorMessage) (en :: ErrorMessage) :: t where
  Unique '[c] e0 en = c
  Unique '[] e0 en = TypeError e0
  Unique cs e0 en = TypeError en

type family Stores w c :: [Type] where
  Stores (K1 q k) c = Elem c (Components k) '[k] '[]
  Stores (M1 p q k) c = Stores k c
  Stores (l :*: r) c = Append (Stores l c) (Stores r c)

data Void1

type family Default (break :: t) (def :: t) :: t where
  Default Void1 def = def
  Default break _ = break

-- Step 2 Find a store by type

-- Count how often a certain record type occurs in a Generic
-- Used to guide instance resolution for finding a store in a world.
type family CountTypes w s :: Nat where
  CountTypes (K1 q s) s = 1
  CountTypes (K1 q w) s = 0
  CountTypes (M1 p q k) s = CountTypes k s
  CountTypes (l :*: r) s = CountTypes l s + CountTypes r s

class GHasField w s where
  askField :: w x -> s

instance GHasField r s => GHasField (M1 p q r) s where
  {-# INLINE askField #-}
  askField = askField . unM1

instance GHasField (K1 p s) s where
  {-# INLINE askField #-}
  askField = unK1

instance GHasTypeProduct (CountTypes l s) (l :*: r) s => GHasField (l :*: r) s where
  {-# INLINE askField #-}
  askField = askFieldProduct @(CountTypes l s)

class GHasTypeProduct (wl :: Nat) w s where
  askFieldProduct :: w x -> s

instance GHasField l s => GHasTypeProduct 1 (l :*: r) s where
  {-# INLINE askFieldProduct #-}
  askFieldProduct (l :*: _) = askField l

instance GHasField r s => GHasTypeProduct 0 (l :*: r) s where
  {-# INLINE askFieldProduct #-}
  askFieldProduct (_ :*: r) = askField r

-- Step 3 Tie them together

type GetStore w c =
  Unique
    (Stores (Rep w) c)
    ( 'Text "Type '"
        ':<>: 'ShowType w
        ':<>: 'Text "' contains no stores for component type '"
        ':<>: 'ShowType c
        ':<>: 'Text "'"
    )
    ( 'Text "Type '"
        ':<>: 'ShowType w
        ':<>: 'Text "' contains more than one store providing component type '"
        ':<>: 'ShowType c
        ':<>: 'Text "':"
        ':$$: 'ShowType (Stores (Rep w) c)
    )

type HasField w t = GHasField (Rep w) t

type HasStore w c = HasField w (GetStore w c)

{-# INLINE focusField #-}
focusField ::
  forall t w m a.
  (Generic w, HasField w t) =>
  ReaderT t m a ->
  ReaderT w m a
focusField = withReaderT (\w -> (askField (from w) :: t))

{-# INLINE focusStore #-}
focusStore ::
  forall c w m a.
  (Generic w, HasStore w c) =>
  ReaderT (GetStore w c) m a ->
  ReaderT w m a
focusStore = focusField
