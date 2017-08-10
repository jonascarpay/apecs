module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M

import Control.ECS.Types

newtype SimpleMap c = SimpleMap c deriving (Eq, Show)
instance Component (SimpleMap c) where
  type Repr    (SimpleMap c) = Maybe c
  type Storage (SimpleMap c) = M.IntMap c
  empty = Store mempty
  slice (Store s)                              = return (Slice (M.keysSet s))
  retrieve (Entity e) (Store s)                = return (Reads (M.lookup e s))
  store (Entity e) (Writes (Just c)) (Store s) = return (Store (M.insert e c s))
  store (Entity e) (Writes Nothing) (Store s)  = return (Store (M.delete e s))

data SimpleFlag
instance Component SimpleFlag where
  type Repr    SimpleFlag = Bool
  type Storage SimpleFlag = S.IntSet
  empty = Store mempty
  slice (Store s)                           = return (Slice s)
  retrieve (Entity e) (Store s)             = return (Reads (S.member e s))
  store (Entity e) (Writes True)  (Store s) = return (Store (S.insert e s))
  store (Entity e) (Writes False) (Store s) = return (Store (S.delete e s))

data EntityCounter
instance Component EntityCounter where
  type Repr EntityCounter = Int
  type Storage EntityCounter = Int
  empty = Store 0
  slice _ = return (Slice (S.singleton (-1)))
  retrieve _ (Store ix) = return (Reads ix)
  store _ (Writes ix) _ = return (Store ix)
