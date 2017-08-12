{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Core where

import qualified Data.IntSet as S
import Control.Monad.State
import GHC.Exts (Constraint)

newtype Entity = Entity Int

class CStorage (Storage c) => Component c where
  type Storage c :: *

class CStorage c where
  type Runtime c :: *
  type Env c (m :: * -> *) :: Constraint

  empty    :: Env c m => m c
  slice    :: Env c m => System c m (Slice a)
  retrieve :: Env c m => Entity -> System c m (Reads c)
  store    :: Env c m => Entity -> Writes c -> System c m ()

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

embed :: (Monad m, Component c, w `Has` c) => System (Store c) m a -> System w m a
embed sys = do w <- get
               (a, c') <- lift $ runSystem sys (getC w)
               modify (putC c')
               return a

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads  (Runtime comp)
newtype Writes comp = Writes (Runtime comp)
newtype Store  comp = Store { unStore :: Storage comp }

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( CStorage a, CStorage b
         ) => CStorage (a, b) where

  type Runtime (a, b) = (Runtime a, Runtime b)

  type Env (a, b) m = (Monad m, Env a m, Env b m)

  empty =
    do sta <- empty
       stb <- empty
       return $ (sta, stb)

  slice =
    do (sta, stb) <- get
       (Slice sla, sta') <- lift $ runSystem slice sta
       (Slice slb, stb') <- lift $ runSystem slice stb
       put (sta', stb')
       return . Slice $ S.intersection sla slb

  retrieve ety =
    do (sta, stb) <- get
       (Reads ra, sta') <- lift $ runSystem (retrieve ety) sta
       (Reads rb, stb') <- lift $ runSystem (retrieve ety) stb
       put (sta', stb')
       return (Reads (ra, rb))

  store ety (Writes (wa, wb)) =
    do (sta, stb) <- get
       sta' <- lift $ execStateT (store ety (Writes wa)) sta
       stb' <- lift $ execStateT (store ety (Writes wb)) stb
       put (sta', stb')

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = let Store sta :: Store a = getC w
               Store stb :: Store b = getC w
            in Store (sta, stb)

  putC (Store (sta, stb)) = putC (Store stb :: Store b) . putC (Store sta :: Store a)
