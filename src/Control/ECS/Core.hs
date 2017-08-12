{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Core where

import qualified Data.IntSet as S
import Control.Monad.State

newtype Entity = Entity Int

class SStorage (Storage c) => Component c where
  type Storage c :: *

class SStorage c where
  type SRuntime c :: *

  sEmpty    :: System s c
  sSlice    :: System c S.IntSet
  sRetrieve :: Entity -> System c (SRuntime c)
  sStore    :: Entity -> SRuntime c -> System c ()

-- Wrapper
type Runtime a = SRuntime (Storage a)

newtype Slice  c = Slice S.IntSet
newtype Reads  c = Reads  (Runtime c)
newtype Writes c = Writes (Runtime c)
newtype Store  c = Store  {unStore :: Storage c}

empty :: Component c => System s (Store c)
empty = Store <$> sEmpty

slice :: Component c => System (Store c) (Slice c)
slice = do Store s <- get
           (a, s') <- runSystem sSlice s
           put (Store s')
           return $ Slice a

retrieve :: Component c => Entity -> System (Storage c) (Reads c)
retrieve e = Reads <$> sRetrieve e

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

class w `Has` c where
  getC :: w -> Store c
  putC :: Store c -> w -> w

newtype System s a = System ( StateT s IO a ) deriving (Functor, Applicative, Monad, MonadIO)
deriving instance MonadState s (System s)

runSystemIO :: System s a -> s -> IO (a, s)
runSystemIO (System st) = runStateT st

runSystem :: System s a -> s -> System w (a, s)
runSystem sys = System . lift . runSystemIO sys

runWith :: s -> System s a -> System w (a, s)
runWith = flip runSystem

global :: Has w c => System (Store c) a -> System w a
global sys = do w <- get
                (a, c') <- runSystem sys (getC w)
                put (putC c' w)
                return a

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance ( SStorage a, SStorage b
         ) => SStorage (a, b) where

  type SRuntime (a, b) = (SRuntime a, SRuntime b)

  sEmpty =
    do sta <- sEmpty
       stb <- sEmpty
       return (sta, stb)

  sSlice =
    do (sta, stb) <- get
       (sla, sta') <- runSystem sSlice sta
       (slb, stb') <- runSystem sSlice stb
       put (sta', stb')
       return $ S.intersection sla slb

  sRetrieve ety =
    do (sta, stb) <- get
       (ra, sta') <- runSystem (sRetrieve ety) sta
       (rb, stb') <- runSystem (sRetrieve ety) stb
       put (sta', stb')
       return (ra, rb)

  sStore ety (wa, wb) =
    do (sta, stb) <- get
       ((),sta') <- runSystem (sStore ety wa) sta
       ((),stb') <- runSystem (sStore ety wb) stb
       put (sta', stb')

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = let Store sa :: Store a = getC w
               Store sb :: Store b = getC w
            in Store (sa, sb)

  putC (Store (sa, sb)) = putC (Store sb :: Store b) . putC (Store sa :: Store a)
