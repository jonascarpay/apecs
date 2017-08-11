{-# LANGUAGE ConstraintKinds, FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Types where

import qualified Data.IntSet as S
import Control.Monad.State
import Control.Lens

newtype Entity = Entity Int

class Monad m => Component m c where

  type Repr    c :: *
  type Storage c :: *

  empty    :: m (Store c)
  slice    :: System (Store c) m (Slice c)
  retrieve :: Entity -> System (Store c) m (Reads c)
  store    :: Entity -> Writes c -> System (Store c) m ()

type System = StateT

runSystem :: System s m a -> s -> m (a, s)
runSystem = runStateT

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads (Repr comp)
newtype Writes comp = Writes (Repr comp)
newtype Store  comp = Store { _unStore :: Storage comp }
makeLenses  ''Store
makeWrapped ''Store

instance (Monad (t m), Component m a, MonadTrans t) => Component (t m) a where

instance (Component m a, Component m b) => Component m (a, b) where

  type Repr    (a, b) = (Repr a, Repr b)
  type Storage (a, b) = (Storage a, Storage b)

  empty =
    do Store sta :: Store a <- empty
       Store stb :: Store b <- empty
       return $ Store (sta, stb)

  slice =
    do Store (sta, stb) <- get
       (Slice sla, Store sta') <- runSystem slice (Store sta :: Store a)
       (Slice slb, Store stb') <- runSystem slice (Store stb :: Store b)
       undefined
