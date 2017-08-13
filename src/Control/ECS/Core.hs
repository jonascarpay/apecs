{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Core where

import qualified Data.IntSet as S
import Control.Monad.State

newtype Entity = Entity Int deriving (Eq, Show)

class SStorage (Storage c) => Component c where
  type Storage c :: *

class SStorage c where
  type SRuntime c :: *

  sEmpty    :: System s c
  sSlice    :: System c S.IntSet
  sRetrieve :: Entity -> System c (SRuntime c)
  sStore    :: Entity -> SRuntime c -> System c ()

newtype System s a = System ( StateT s IO a ) deriving (Functor, Applicative, Monad, MonadIO)
deriving instance MonadState s (System s)

runSystemIO :: System s a -> s -> IO (a, s)
runSystemIO (System st) = runStateT st

runSystem :: System s a -> s -> System w (a, s)
runSystem sys = System . lift . runSystemIO sys

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
