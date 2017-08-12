{-# LANGUAGE ConstraintKinds, FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Types where

import qualified Data.IntSet as S
import Control.Monad.State

newtype Entity = Entity Int

class Representable c where
  type Runtime c :: *
  type Storage c :: *

class (Representable c, Monad m) => Component m c where
  empty    :: m (Store c)
  slice    :: System (Store c) m (Slice c)
  retrieve :: Entity -> System (Store c) m (Reads c)
  store    :: Entity -> Writes c -> System (Store c) m ()

class a `Has` b where

type System = StateT

runSystem :: System s m a -> s -> m (a, s)
runSystem = runStateT

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads  (Runtime comp)
newtype Writes comp = Writes (Runtime comp)
newtype Store  comp = Store { unStore :: Storage comp }

instance (Monad (t m), MonadTrans t, Component m a) => Component (t m) a where
  empty    = lift empty
  slice    = slice
  retrieve = retrieve
  store    = store

instance (Representable a, Representable b) => Representable (a, b) where
  type Runtime (a, b) = (Runtime a, Runtime b)
  type Storage (a, b) = (Storage a, Storage b)

instance ( Component m a
         , Component m b
         ) => Component m (a, b) where

  empty =
    do Store sta :: Store a <- empty
       Store stb :: Store b <- empty
       return $ Store (sta, stb)

  slice =
    do Store (sta, stb) <- get
       (Slice sla, Store sta') <- runSystem slice (Store sta :: Store a)
       (Slice slb, Store stb') <- runSystem slice (Store stb :: Store b)
       put (Store (sta', stb'))
       return . Slice $ S.intersection sla slb

  retrieve ety =
    do Store (sta, stb) <- get
       (Reads ra, Store sta') <- runSystem (retrieve ety) (Store sta :: Store a)
       (Reads rb, Store stb') <- runSystem (retrieve ety) (Store stb :: Store b)
       put (Store (sta', stb'))
       return (Reads (ra, rb))

  store ety (Writes (wa, wb)) =
    do Store (sta, stb) <- get
       (Store sta') <- execStateT (store ety (Writes wa)) (Store sta :: Store a)
       (Store stb') <- execStateT (store ety (Writes wb)) (Store stb :: Store b)
       put (Store (sta', stb'))
