{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Control.ECS.Types where

import qualified Data.IntSet as S
import Control.Monad.State
import Control.Lens

newtype Entity = Entity Int

class Component comp where

  type Repr    comp :: *
  type Storage comp :: *

  empty    :: Store comp
  slice    :: System (Store comp) (Slice comp)
  retrieve :: Entity -> System (Store comp) (Reads comp)
  store    :: Entity -> Writes comp -> System (Store comp) ()

newtype System s a = System {runSystem :: StateT s IO a}
deriving instance Functor      (System s)
deriving instance Applicative  (System s)
deriving instance Monad        (System s)
deriving instance MonadIO      (System s)
deriving instance MonadState s (System s)

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads (Repr comp)
newtype Writes comp = Writes (Repr comp)
newtype Store  comp = Store { _unStore :: Storage comp }
makeLenses ''Store
makeWrapped ''Store

runSystem' :: System s a -> s -> System c (a, s)
runSystem' sys = liftIO . runStateT (runSystem sys)

instance (Component a, Component b) => Component (a, b) where

  type Repr    (a, b) = (Repr a, Repr b)
  type Storage (a, b) = (Storage a, Storage b)

  empty =
    let Store sta :: Store a = empty
        Store stb :: Store b = empty
     in Store (sta, stb)

  slice =
    do Store (sta, stb) <- get
       (Slice sla, Store sta') <- runSystem' slice (Store sta :: Store a)
       (Slice slb, Store stb') <- liftIO . runStateT (runSystem slice) $ (Store stb :: Store b)
       put (Store (sta', stb'))
       return $ Slice (sla `S.intersection` slb)

  retrieve ety =
    do Store (sta, stb) <- get
       (Reads ra, Store sta') <- liftIO . runStateT (runSystem $ retrieve ety) $ (Store sta :: Store a)
       undefined
