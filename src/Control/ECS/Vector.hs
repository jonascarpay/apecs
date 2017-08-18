module Control.ECS.Vector where

data V2 a = V2 {-# UNPACK #-} a {-# UNPACK #-} a
data V3 a = V3 {-# UNPACK #-} a {-# UNPACK #-} a {-# UNPACK #-} a

class Vec a where
  type VElem a :: *
  (.+) :: a -> a -> a
  (.-) :: a -> a -> a
  (.*) :: VElem a -> a -> a
  (./) :: VElem a -> a -> a
  dot :: a -> a -> a
