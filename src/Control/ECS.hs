{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}

module Control.ECS (
  module Control.ECS.Core,
  module Control.ECS,
  module Control.ECS.Immutable,
) where

import qualified Data.IntSet as S

import Control.ECS.Core
import Control.ECS.Immutable
import Control.Monad.State

type Runtime a = SRuntime (Storage a)

newtype Slice  c = Slice S.IntSet
newtype Reads  c = Reads  {unReads  :: Runtime c}
newtype Writes c = Writes {unWrites :: Runtime c}
newtype Store  c = Store  {unStore  :: Storage c}

class w `Has` c where
  getC :: w -> Store c
  putC :: Store c -> w -> w

empty :: Component c => System s (Store c)
empty = Store <$> sEmpty

slice :: Component c => System (Store c) (Slice c)
slice = do Store s <- get
           (a, s') <- runSystem sSlice s
           put (Store s')
           return $ Slice a

retrieve :: Component c => Entity -> System (Store c) (Reads c)
retrieve e = do Store s <- get
                (r, s') <- runSystem (sRetrieve e) s
                put (Store s')
                return $ Reads r

store :: Component c => Entity -> Writes c -> System (Store c) ()
store e (Writes w) = do Store s <- get
                        ((), s') <- runSystem (sStore e w) s
                        put (Store s')

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

global :: Has w c => System (Store c) a -> System w a
global sys = do w <- get
                (a, c') <- runSystem sys (getC w)
                put (putC c' w)
                return a

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = let Store sa :: Store a = getC w
               Store sb :: Store b = getC w
            in Store (sa, sb)

  putC (Store (sa, sb)) = putC (Store sb :: Store b) . putC (Store sa :: Store a)

runWith :: s -> System s a -> System w (a, s)
runWith = flip runSystem

getEntityCount :: System (Store EntityCounter) Int
getEntityCount = do unReads <$> retrieve undefined

newEntity :: Has w EntityCounter => System w Entity
newEntity = global $ do (Reads c :: Reads EntityCounter) <- retrieve undefined
                        store undefined (Writes (c+1))
                        return (Entity c)

withDomain :: (Has w a, Has w b, Component a, Component b) => (a -> b) -> System w ()
withDomain = undefined
