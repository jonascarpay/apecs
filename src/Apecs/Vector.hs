-- | A lightweight version of Edward Kmett's linear, included for convenience' sake

{-# LANGUAGE TypeFamilyDependencies, ScopedTypeVariables, FlexibleContexts #-}

module Apecs.Vector where

import Control.Applicative

class VecSimple a where
  type VecElem a
  dot :: a -> a -> VecElem a
  vlength :: a -> VecElem a
  normalize :: a -> a

-- V2
data V2 a = V2 !a !a deriving (Eq, Show)

instance Functor V2 where
  {-# INLINE fmap #-}
  fmap f (V2 a b) = V2 (f a) (f b)

instance Applicative V2 where
  {-# INLINE (<*>) #-}
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)
  {-# INLINE pure #-}
  pure x = V2 x x

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V2 a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => VecSimple (V2 a) where
  type VecElem (V2 a) = a
  {-# INLINE dot #-}
  V2 a b `dot` V2 c d = a*c + b*d
  vlength (V2 a b) = sqrt (a*a + b*b)
  normalize vec = vec / pure (vlength vec)

-- V3
data V3 a = V3 !a !a !a deriving (Eq, Show)

instance Functor V3 where
  {-# INLINE fmap #-}
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)

instance Applicative V3 where
  {-# INLINE (<*>) #-}
  V3 fx fy fz <*> V3 x y z = V3 (fx x) (fy y) (fz z)
  {-# INLINE pure #-}
  pure x = V3 x x x

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V3 a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => VecSimple (V3 a) where
  type VecElem (V3 a) = a
  {-# INLINE dot #-}
  V3 a b c `dot` V3 d e f = a*d + b*e + c*f
  vlength (V3 a b c) = sqrt (a*a + b*b + c*c)
  normalize vec = vec / pure (vlength vec)

{-# INLINE outer #-}
outer :: Num a => V3 a -> V3 a -> V3 a
V3 a b c `outer` V3 d e f = V3 (b*f - e*c) (c*d - a*f) (a*e - b*d)
