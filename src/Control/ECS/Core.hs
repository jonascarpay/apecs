{-# LANGUAGE ConstraintKinds, FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Core where

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

class world `Has` component where
  getC :: world -> Store component
  putC :: Store component -> world -> world

type System = StateT

runSystem :: System s m a -> s -> m (a, s)
runSystem = runStateT

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

embed :: (Component m c, w `Has` c) => System (Store c) m a -> System w m a
embed sys = do w <- get
               (a, c') <- lift $ runSystem sys (getC w)
               modify (putC c')
               return a

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads  (Runtime comp)
newtype Writes comp = Writes (Runtime comp)
newtype Store  comp = Store { unStore :: Storage comp }

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
       (Slice sla, Store sta') <- lift $ runSystem slice (Store sta :: Store a)
       (Slice slb, Store stb') <- lift $ runSystem slice (Store stb :: Store b)
       put (Store (sta', stb'))
       return . Slice $ S.intersection sla slb

  retrieve ety =
    do Store (sta, stb) <- get
       (Reads ra, Store sta') <- lift $ runSystem (retrieve ety) (Store sta :: Store a)
       (Reads rb, Store stb') <- lift $ runSystem (retrieve ety) (Store stb :: Store b)
       put (Store (sta', stb'))
       return (Reads (ra, rb))

  store ety (Writes (wa, wb)) =
    do Store (sta, stb) <- get
       (Store sta') <- lift $ execStateT (store ety (Writes wa)) (Store sta :: Store a)
       (Store stb') <- lift $ execStateT (store ety (Writes wb)) (Store stb :: Store b)
       put (Store (sta', stb'))

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = let Store sta :: Store a = getC w
               Store stb :: Store b = getC w
            in Store (sta, stb)

  putC (Store (sta, stb)) = putC (Store stb :: Store b) . putC (Store sta :: Store a)
