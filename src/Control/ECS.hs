{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Control.ECS (
  module Control.ECS.Types,
  module Control.ECS,
) where

import Control.ECS.Types
import Control.Monad

import qualified Data.IntSet as S

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = Slice (s1 `S.union` s2)

toList :: Slice c -> [Entity]
toList (Slice s) = fmap Entity (S.toList s)

mapC :: forall a b w. (w `Stores` a, w `Stores` b) => (Reads a -> Writes b) -> w -> System w
mapC f w = do let sta :: Store a = getStore w
                  stb :: Store b = getStore w

              sl <- toList <$> slice (getStore w :: Store a)
              c' <- forM sl $ \e -> do c <- retrieve e sta
                                       store e 

              return w

