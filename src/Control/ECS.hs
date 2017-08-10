{-# LANGUAGE TypeOperators #-}

module Control.ECS (
  module Control.ECS.Types,
  module Control.ECS,
) where

import Control.ECS.Types

import qualified Data.IntSet as S

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

mapC :: (w `Stores` a, w `Stores` b) => (a -> b) -> w -> System w
mapC = undefined
