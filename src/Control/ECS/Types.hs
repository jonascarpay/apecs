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

newtype System s a = System { runSystem :: StateT s IO a }
deriving instance Functor      (System s)
deriving instance Applicative  (System s)
deriving instance Monad        (System s)
deriving instance MonadState s (System s)

newtype Slice  comp = Slice S.IntSet
newtype Reads  comp = Reads (Repr comp)
newtype Writes comp = Writes (Repr comp)
newtype Store  comp = Store { _unStore :: Storage comp }
makeLenses ''Store


