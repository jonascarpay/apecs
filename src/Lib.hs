module Lib (
    module Lib,
    module Control.Monad.Trans.State.Strict
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

type System s a  = State s a
newtype Entity c = Entity Int
type GSystemT = StateT
type ESystemT cs = StateT (Repr cs)

liftE :: (WComp w cs, Monad m) => Entity cs -> ESystemT cs (GSystemT w m) a -> GSystemT w m a
liftE e sys = do cs <- retrieve e
                 (a, cs') <- runStateT sys cs
                 store e cs'
                 return a

class World w => WComp w c where
  type Repr c :: *

  retrieve :: Monad m => Entity c -> GSystemT w m (Repr c)
  store    :: Monad m => Entity c -> Repr c -> GSystemT w m ()

class World w where
  empty   :: w
  create  :: (Monad m, WComp w c) => Repr c -> GSystemT w m (Entity c)
  destroy :: Entity c -> GSystemT w m ()

instance (WComp w a, WComp w b) => WComp w (a, b) where
  type Repr (a, b) = (Repr a, Repr b)
  store (Entity e) (a, b) = do store (Entity e :: Entity a) a
                               store (Entity e :: Entity b) b
  retrieve (Entity e) = do a <- retrieve (Entity e :: Entity a)
                           b <- retrieve (Entity e :: Entity b)
                           return (a, b)



-- Niet noodzakelijk:
instance (WComp w a, WComp w b, WComp w c) => WComp w (a, b, c) where
  type Repr (a, b, c) = (Repr a, Repr b, Repr c)

  store (Entity e) (a, b, c) = do store (Entity e :: Entity a) a
                                  store (Entity e :: Entity b) b
                                  store (Entity e :: Entity c) c

  retrieve (Entity e) = do a <- retrieve (Entity e :: Entity a)
                           b <- retrieve (Entity e :: Entity b)
                           c <- retrieve (Entity e :: Entity c)
                           return (a, b, c)

