{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Apecs.Core
  ( Entity (..),
    SystemT,
    System,
    Get (..),
    Set (..),
    Destroy (..),
    Members (..),
    Initialize (..),
    Not (..),
  )
where

import Apecs.Focus
import Control.Applicative
import Control.Monad.Reader
import Data.IntSet as IS
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import GHC.Generics

-- Core

newtype Entity = Entity {unEntity :: Int}
  deriving (Eq, Show, V.Storable)

type SystemT w m a = ReaderT w m a

type System w a = ReaderT w IO a

-- law:
-- no side-effects: get ety >> get ety = get ety
class Get w m c where
  exists :: Proxy c -> Entity -> SystemT w m Bool
  get :: Entity -> SystemT w m c

  default exists :: GGet w m (Rep c) => Proxy c -> Entity -> SystemT w m Bool
  {-# INLINE exists #-}
  exists _ ety = gexists (Proxy @(Rep c)) ety
  default get :: (Functor m, Generic c, GGet w m (Rep c)) => Entity -> SystemT w m c
  {-# INLINE get #-}
  get ety = to <$> gget ety

class Set w m c where
  set :: c -> Entity -> SystemT w m ()
  default set :: (Generic c, GSet w m (Rep c)) => c -> Entity -> SystemT w m ()
  {-# INLINE set #-}
  set c ety = gset (from c) ety

class Destroy w m c where
  destroy :: Proxy c -> Entity -> SystemT w m ()
  default destroy :: GDestroy w m (Rep c) => Proxy c -> Entity -> SystemT w m ()
  {-# INLINE destroy #-}
  destroy _ ety = gdestroy (Proxy @(Rep c)) ety

class Members w m c where
  members :: Proxy c -> SystemT w m (Vector Entity)
  default members :: GMembers w m (Rep c) => Proxy c -> SystemT w m (Vector Entity)
  {-# INLINE members #-}
  members _ = gmembers (Proxy @(Rep c))

class Initialize m w where
  initialize :: m w
  default initialize :: (Functor m, Generic w, GInitialize m (Rep w)) => m w
  {-# INLINE initialize #-}
  initialize = to <$> ginitialize

-- Generic Core classes
-- These are used to derive instances for _Components_

class GGet w m c where
  gexists :: Proxy c -> Entity -> SystemT w m Bool
  gget :: Entity -> SystemT w m (c x)

class GSet w m c where
  gset :: c x -> Entity -> SystemT w m ()

class GDestroy w m c where
  gdestroy :: Proxy c -> Entity -> SystemT w m ()

class GMembers w m c where
  gmembers :: Proxy c -> SystemT w m (Vector Entity)

class GInitialize m w where
  ginitialize :: m (w x)

-- Magic Instances
-- These define instances for _Stores_
-- "Magic" because overlappable and undecidable

instance {-# OVERLAPPABLE #-} (Generic w, HasStore w c, Get (GetStore w c) m c) => Get w m c where
  {-# INLINE exists #-}
  exists p ety = focusStore @c $ exists p ety
  {-# INLINE get #-}
  get ety = focusStore @c $ get ety

instance {-# OVERLAPPABLE #-} (Generic w, HasStore w c, Set (GetStore w c) m c) => Set w m c where
  {-# INLINE set #-}
  set a ety = focusStore @c $ set a ety

instance {-# OVERLAPPABLE #-} (Generic w, HasStore w c, Destroy (GetStore w c) m c) => Destroy w m c where
  {-# INLINE destroy #-}
  destroy p ety = focusStore @c $ destroy p ety

instance {-# OVERLAPPABLE #-} (Generic w, HasStore w c, Members (GetStore w c) m c) => Members w m c where
  {-# INLINE members #-}
  members p = focusStore @c $ members p

-- Generic Instances

--- Get
instance (Functor m, GGet w m c) => GGet w m (M1 p q c) where
  {-# INLINE gexists #-}
  gexists _ ety = gexists (Proxy @c) ety
  {-# INLINE gget #-}
  gget ety = M1 <$> gget ety

instance (Functor m, Get w m c) => GGet w m (K1 p c) where
  {-# INLINE gexists #-}
  gexists _ ety = exists (Proxy @c) ety
  {-# INLINE gget #-}
  gget ety = K1 <$> get ety

instance (Applicative m, GGet w m l, GGet w m r) => GGet w m (l :*: r) where
  {-# INLINE gexists #-}
  gexists _ ety = liftA2 (&&) (gexists (Proxy @l) ety) (gexists (Proxy @r) ety)
  {-# INLINE gget #-}
  gget ety = liftA2 (:*:) (gget ety) (gget ety)

instance (Monad m, GGet w m l, GGet w m r) => GGet w m (l :+: r) where
  {-# INLINE gexists #-}
  gexists _ ety = do
    rExists <- gexists (Proxy @r) ety
    if rExists
      then pure True
      else gexists (Proxy @l) ety
  {-# INLINE gget #-}
  gget ety = do
    rExists <- gexists (Proxy @r) ety
    if rExists
      then R1 <$> gget ety
      else L1 <$> gget ety

instance Applicative m => GGet w m U1 where
  {-# INLINE gexists #-}
  gexists _ _ = pure True
  {-# INLINE gget #-}
  gget _ = pure U1

instance Applicative m => GGet w m V1 where
  {-# INLINE gexists #-}
  gexists _ _ = pure False
  {-# INLINE gget #-}
  gget (Entity ety) = error $ "Querying void value for entity " <> show ety

--- Set
instance GSet w m c => GSet w m (M1 p q c) where
  {-# INLINE gset #-}
  gset (M1 c) ety = gset c ety

instance Set w m c => GSet w m (K1 p c) where
  {-# INLINE gset #-}
  gset (K1 c) ety = set c ety

instance (Applicative m, GSet w m l, GSet w m r) => GSet w m (l :*: r) where
  {-# INLINE gset #-}
  gset (l :*: r) ety = gset l ety *> gset r ety

instance (GSet w m l, GSet w m r) => GSet w m (l :+: r) where
  {-# INLINE gset #-}
  gset (R1 r) ety = gset r ety
  gset (L1 l) ety = gset l ety

instance GSet w m V1 where
  {-# INLINE gset #-}
  gset v = case v of

--- Destroy
instance GDestroy w m c => GDestroy w m (M1 p q c) where
  {-# INLINE gdestroy #-}
  gdestroy _ ety = gdestroy (Proxy @c) ety

instance Destroy w m c => GDestroy w m (K1 p c) where
  {-# INLINE gdestroy #-}
  gdestroy _ ety = destroy (Proxy @c) ety

instance (Applicative m, GDestroy w m l, GDestroy w m r) => GDestroy w m (l :*: r) where
  {-# INLINE gdestroy #-}
  gdestroy _ ety = gdestroy (Proxy @l) ety *> gdestroy (Proxy @r) ety

instance Applicative m => GDestroy w m V1 where
  {-# INLINE gdestroy #-}
  gdestroy _ _ = pure ()

--- Members
instance GMembers w m c => GMembers w m (M1 p q c) where
  {-# INLINE gmembers #-}
  gmembers _ = gmembers (Proxy @c)

instance Members w m c => GMembers w m (K1 p c) where
  {-# INLINE gmembers #-}
  gmembers _ = members (Proxy @c)

instance (GMembers w m l, GGet w m r, Monad m) => GMembers w m (l :*: r) where
  {-# INLINE gmembers #-}
  gmembers _ = gmembers (Proxy @l) >>= V.filterM (gexists (Proxy @r))

instance (Applicative m, GMembers w m l, GMembers w m r) => GMembers w m (l :+: r) where
  {-# INLINE gmembers #-}
  gmembers _ =
    liftA2
      (\l r -> V.fromList . fmap Entity . IS.toList $ l <> r)
      (IS.fromList . fmap unEntity . V.toList <$> gmembers (Proxy @l))
      (IS.fromList . fmap unEntity . V.toList <$> gmembers (Proxy @r))

instance Applicative m => GMembers w m V1 where
  {-# INLINE gmembers #-}
  gmembers _ = pure V.empty

-- Initialize
instance (GInitialize m w, Functor m) => GInitialize m (M1 p q w) where
  {-# INLINE ginitialize #-}
  ginitialize = M1 <$> ginitialize

instance (Initialize m w, Functor m) => GInitialize m (K1 p w) where
  {-# INLINE ginitialize #-}
  ginitialize = K1 <$> initialize

instance (GInitialize m l, GInitialize m r, Applicative m) => GInitialize m (l :*: r) where
  {-# INLINE ginitialize #-}
  ginitialize = liftA2 (:*:) ginitialize ginitialize

instance Applicative m => GInitialize m U1 where
  {-# INLINE ginitialize #-}
  ginitialize = pure U1

-- Instances

--- ()
instance Applicative m => Get w m ()

instance Applicative m => Set w m () where set _ _ = pure ()

--- Maybe
instance (Get w m c, Monad m) => Get w m (Maybe c) where exists _ _ = pure True

instance (Destroy w m c, Set w m c) => Set w m (Maybe c) where
  {-# INLINE set #-}
  set Nothing ety = destroy (Proxy @c) ety
  set (Just c) ety = set c ety

--- Entity
instance Applicative m => Get w m Entity where
  {-# INLINE get #-}
  get ety = pure ety
  {-# INLINE exists #-}
  exists _ _ = pure True

--- Not
data Not a = Not
  deriving (Eq, Show, Generic)

instance (Applicative m, Get w m c) => Get w m (Not c) where
  {-# INLINE exists #-}
  exists _ ety = not <$> exists (Proxy @c) ety

instance Destroy w m c => Set w m (Not c) where
  {-# INLINE set #-}
  set _ ety = destroy (Proxy @c) ety

{- ORMOLU_DISABLE -}

-- Either

instance (Get w m a, Get w m b, Monad m) => Get w m (Either a b)
instance (Members w m a, Members w m b, Monad m) => Members w m (Either a b)

--- Tuples
instance (Get w m a, Get w m b, Applicative m) => Get w m (a, b)
instance (Set w m a, Set w m b, Applicative m) => Set w m (a, b)
instance (Destroy w m a, Destroy w m b, Applicative m) => Destroy w m (a, b)
instance (Members w m a, Get w m b, Monad m) => Members w m (a, b)
instance (Initialize m a, Initialize m b, Applicative m) => Initialize m (a, b)

instance (Get w m a, Get w m b, Get w m c, Applicative m) => Get w m (a, b, c)
instance (Set w m a, Set w m b, Set w m c, Applicative m) => Set w m (a, b, c)
instance (Destroy w m a, Destroy w m b, Destroy w m c, Applicative m) => Destroy w m (a, b, c)
instance (Members w m a, Get w m b, Get w m c, Monad m) => Members w m (a, b, c)
instance (Initialize m a, Initialize m b, Initialize m c, Applicative m) => Initialize m (a, b, c)

instance (Get w m a, Get w m b, Get w m c, Get w m d, Applicative m) => Get w m (a, b, c, d)
instance (Set w m a, Set w m b, Set w m c, Set w m d, Applicative m) => Set w m (a, b, c, d)
instance (Destroy w m a, Destroy w m b, Destroy w m c, Destroy w m d, Applicative m) => Destroy w m (a, b, c, d)
instance (Members w m a, Get w m b, Get w m c, Get w m d, Monad m) => Members w m (a, b, c, d)
instance (Initialize m a, Initialize m b, Initialize m c, Initialize m d, Applicative m) => Initialize m (a, b, c, d)

instance (Get w m a, Get w m b, Get w m c, Get w m d, Get w m e, Applicative m) => Get w m (a, b, c, d, e)
instance (Set w m a, Set w m b, Set w m c, Set w m d, Set w m e, Applicative m) => Set w m (a, b, c, d, e)
instance (Destroy w m a, Destroy w m b, Destroy w m c, Destroy w m d, Destroy w m e, Applicative m) => Destroy w m (a, b, c, d, e)
instance (Members w m a, Get w m b, Get w m c, Get w m d, Get w m e, Monad m) => Members w m (a, b, c, d, e)
instance (Initialize m a, Initialize m b, Initialize m c, Initialize m d, Initialize m e, Applicative m) => Initialize m (a, b, c, d, e)

instance (Get w m a, Get w m b, Get w m c, Get w m d, Get w m e, Get w m f, Applicative m) => Get w m (a, b, c, d, e, f)
instance (Set w m a, Set w m b, Set w m c, Set w m d, Set w m e, Set w m f, Applicative m) => Set w m (a, b, c, d, e, f)
instance (Destroy w m a, Destroy w m b, Destroy w m c, Destroy w m d, Destroy w m e, Destroy w m f, Applicative m) => Destroy w m (a, b, c, d, e, f)
instance (Members w m a, Get w m b, Get w m c, Get w m d, Get w m e, Get w m f, Monad m) => Members w m (a, b, c, d, e, f)
instance (Initialize m a, Initialize m b, Initialize m c, Initialize m d, Initialize m e, Initialize m f, Applicative m) => Initialize m (a, b, c, d, e, f)

instance (Get w m a, Get w m b, Get w m c, Get w m d, Get w m e, Get w m f, Get w m g, Applicative m) => Get w m (a, b, c, d, e, f, g)
instance (Set w m a, Set w m b, Set w m c, Set w m d, Set w m e, Set w m f, Set w m g, Applicative m) => Set w m (a, b, c, d, e, f, g)
instance (Destroy w m a, Destroy w m b, Destroy w m c, Destroy w m d, Destroy w m e, Destroy w m f, Destroy w m g, Applicative m) => Destroy w m (a, b, c, d, e, f, g)
instance (Members w m a, Get w m b, Get w m c, Get w m d, Get w m e, Get w m f, Get w m g, Monad m) => Members w m (a, b, c, d, e, f, g)
instance (Initialize m a, Initialize m b, Initialize m c, Initialize m d, Initialize m e, Initialize m f, Initialize m g, Applicative m) => Initialize m (a, b, c, d, e, f, g)
