{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Core where

import qualified Data.IntSet as S
import Control.Monad.State

newtype Entity = Entity Int

class CStorage (Storage c) => Component c where
  type Storage c :: *

class CStorage c where
  type Runtime c :: *

  empty    :: System s c
  slice    :: System c (Slice a)
  retrieve :: Entity -> System c (Reads c)
  store    :: Entity -> Writes c -> System c ()

class world `Has` component where
  getC :: world -> Store component
  putC :: Store component -> world -> world

newtype System s a = System ( StateT s IO a ) deriving (Functor, Applicative, Monad)
deriving instance MonadState s (System s)

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

runSystem :: System s a -> s -> IO (a, s)
runSystem (System st) = runStateT st

embed :: System s a -> s -> System w (a, s)
embed sys = System . lift . runSystem sys

runWith :: s -> System s a -> System w (a, s)
runWith = flip embed

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads  (Runtime comp)
newtype Writes comp = Writes (Runtime comp)
newtype Store  comp = Store { unStore :: Storage comp }

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( CStorage a, CStorage b
         ) => CStorage (a, b) where

  type Runtime (a, b) = (Runtime a, Runtime b)

  empty =
    do sta <- empty
       stb <- empty
       return $ (sta, stb)

  slice =
    do (sta, stb) <- get
       (Slice sla, sta') <- embed slice sta
       (Slice slb, stb') <- embed slice stb
       put (sta', stb')
       return . Slice $ S.intersection sla slb

  retrieve ety =
    do (sta, stb) <- get
       (Reads ra, sta') <- embed (retrieve ety) sta
       (Reads rb, stb') <- embed (retrieve ety) stb
       put (sta', stb')
       return (Reads (ra, rb))

  store ety (Writes (wa, wb)) =
    do (sta, stb) <- get
       ((),sta') <- embed (store ety (Writes wa)) sta
       ((),stb') <- embed (store ety (Writes wb)) stb
       put (sta', stb')

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = let Store sta :: Store a = getC w
               Store stb :: Store b = getC w
            in Store (sta, stb)

  putC (Store (sta, stb)) = putC (Store stb :: Store b) . putC (Store sta :: Store a)
