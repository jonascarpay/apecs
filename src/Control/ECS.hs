module Control.ECS (
  module Control.ECS.Types,
  module Control.ECS.Instances,
) where

import Control.ECS.Types
import Control.ECS.Instances

import qualified Data.IntSet as S

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

