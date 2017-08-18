-- | A default vector module since I suspect most ECS will end up using vectors. If you want more elaborate vectors, try the linear package.
module Control.ECS.Vector where

data V2 a = V2 {-# UNPACK #-} a {-# UNPACK #-} a
  deriving (Eq, Show)
data V3 a = V3 {-# UNPACK #-} a {-# UNPACK #-} a {-# UNPACK #-} a
  deriving (Eq, Show)

class SimpleVec a where
  type VElem a :: *
  (.+) :: a -> a -> a
  (.-) :: a -> a -> a
  (.*) :: VElem a -> a -> a
  dot :: a -> a -> VElem a
  zero :: a

instance Num a => SimpleVec (V2 a) where
  type VElem (V2 a) = a
  V2 x1 y1 .+ V2 x2 y2 = V2 (x1 + x2) (y1+y2)
  V2 x1 y1 .- V2 x2 y2 = V2 (x1 - x2) (y1-y2)
  c .* V2 x y = V2 (c*x) (c*y)
  V2 x1 y1 `dot` V2 x2 y2 = x1 * x2 + y1 * y2
  zero = V2 0 0

instance Num a => SimpleVec (V3 a) where
  type VElem (V3 a) = a
  V3 x1 y1 z1 .+ V3 x2 y2 z2 = V3 (x1 + x2) (y1+y2) (z1+z2)
  V3 x1 y1 z1 .- V3 x2 y2 z2 = V3 (x1 - x2) (y1-y2) (z1-z2)
  c .* V3 x y z = V3 (c*x) (c*y) (c*z)
  V3 x1 y1 z1 `dot` V3 x2 y2 z2 = x1*x2 + y1*y2 + z1*z2
  zero = V3 0 0 0
