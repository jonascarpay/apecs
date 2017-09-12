-- | A lightweight version of Edward Kmett's linear, included for convenience' sake

{-# LANGUAGE TypeFamilyDependencies, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Apecs.Vector where

import Control.Applicative

{-# INLINE dot #-}
dot :: (Num (v a), Num a, Foldable v) => v a -> v a -> a
dot a b = sum $ a * b

{-# INLINE vlength #-}
vlength :: (Foldable v, Num (v a), Floating a) => v a -> a
vlength a = sqrt (dot a a)

{-# INLINE normalize #-}
normalize :: (Num (v b), Floating b, Foldable v, Functor f) => v b -> f b -> f b
normalize v = fmap (/vlength v)

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

instance Foldable V2 where
  foldMap f (V2 x y)    = f x `mappend` f y
  foldr f seed (V2 x y) = f x (f y seed)
  foldr1 f (V2 x y)     = f x y
  foldl f seed (V2 x y) = f (f seed x) y
  foldl1 f (V2 x y)     = f x y
  null _                = False
  length _              = 2
  elem a (V2 x y)       = x == a || y == a
  minimum (V2 x y)      = min x y
  maximum (V2 x y)      = max x y
  sum (V2 x y)          = x + y
  product (V2 x y)      = x * y
  {-# INLINE foldMap #-}
  {-# INLINE foldr #-}
  {-# INLINE foldr1 #-}
  {-# INLINE foldl #-}
  {-# INLINE foldl1 #-}
  {-# INLINE null #-}
  {-# INLINE length #-}
  {-# INLINE elem #-}
  {-# INLINE minimum #-}
  {-# INLINE maximum #-}
  {-# INLINE product #-}
  {-# INLINE sum #-}

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

instance Foldable V3 where
  foldMap f (V3 x y z)    = f x `mappend` f y `mappend` f z
  foldr f seed (V3 x y z) = f x (f y (f z seed))
  foldr1 f (V3 x y z)     = f x (f y z)
  foldl f seed (V3 x y z) = f (f (f seed x) y) z
  foldl1 f (V3 x y z)     = f (f x y) z
  null _                  = False
  length _                = 3
  elem a (V3 x y z)       = x == a || y == a || z == a
  minimum (V3 x y z)      = min (min x y) z
  maximum (V3 x y z)      = max (max x y) z
  sum (V3 x y z)          = x + y + z
  product (V3 x y z)      = x * y * z
  {-# INLINE foldMap #-}
  {-# INLINE foldr #-}
  {-# INLINE foldr1 #-}
  {-# INLINE foldl #-}
  {-# INLINE foldl1 #-}
  {-# INLINE null #-}
  {-# INLINE length #-}
  {-# INLINE elem #-}
  {-# INLINE minimum #-}
  {-# INLINE maximum #-}
  {-# INLINE product #-}
  {-# INLINE sum #-}

{-# INLINE outer #-}
outer :: Num a => V3 a -> V3 a -> V3 a
V3 a b c `outer` V3 d e f = V3 (b*f - e*c) (c*d - a*f) (a*e - b*d)

