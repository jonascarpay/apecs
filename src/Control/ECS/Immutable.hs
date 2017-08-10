module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M

import Control.ECS.Types

newtype SimpleMap c = SimpleMap c

instance Component (SimpleMap c) where
  type Repr    (SimpleMap c) = Maybe c
  type Storage (SimpleMap c) = M.IntMap c
  empty = Store mempty
  slice (Store m) = return (Slice (M.keysSet m))
  retrieve (Entity e) (Store m) = return (Reads (M.lookup e m))
  store (Entity e) (Writes (Just c)) (Store m) = return (Store (M.insert e c m))
  store (Entity e) (Writes Nothing) (Store m) = return (Store (M.delete e m))
