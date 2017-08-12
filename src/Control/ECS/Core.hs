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
  retrieve :: Entity -> System c (Reads (Runtime c))
  store    :: Entity -> Writes c -> System c ()


newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads  (Runtime comp)
newtype Writes comp = Writes (Runtime comp)
newtype Store  comp = Store { unStore :: Storage comp }

slice' :: CStorage (Storage c) => System (Storage c) (Slice c)
slice' = slice

retrieve' :: CStorage (Storage c) => Entity -> System (Storage c) (Reads c)
retrieve' = retrieve

type Runtime' c = Runtime (Storage c)

class w `Has` c where
  getC :: w -> c
  putC :: c -> w -> w

newtype System s a = System ( StateT s IO a ) deriving (Functor, Applicative, Monad, MonadIO)
deriving instance MonadState s (System s)

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

runSystemIO :: System s a -> s -> IO (a, s)
runSystemIO (System st) = runStateT st

runSystem :: System s a -> s -> System w (a, s)
runSystem sys = System . lift . runSystemIO sys

runWith :: s -> System s a -> System w (a, s)
runWith = flip runSystem

embed :: Has w c => System c a -> System w a
embed sys = do w <- get
               (a, c') <- runSystem sys (getC w)
               put (putC c' w)
               return a

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( CStorage a, CStorage b
         ) => CStorage (a, b) where

  type Runtime (a, b) = (Runtime a, Runtime b)

  empty =
    do sta <- empty
       stb <- empty
       return (sta, stb)

  slice =
    do (sta, stb) <- get
       (Slice sla, sta') <- runSystem slice sta
       (Slice slb, stb') <- runSystem slice stb
       put (sta', stb')
       return . Slice $ S.intersection sla slb

  retrieve ety =
    do (sta, stb) <- get
       (Reads ra, sta') <- runSystem (retrieve ety) sta
       (Reads rb, stb') <- runSystem (retrieve ety) stb
       put (sta', stb')
       return (Reads (ra, rb))

  store ety (Writes (wa, wb)) =
    do (sta, stb) <- get
       ((),sta') <- runSystem (store ety (Writes wa)) sta
       ((),stb') <- runSystem (store ety (Writes wb)) stb
       put (sta', stb')

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = (getC w, getC w)
  putC (a, b) = putC b . putC a
