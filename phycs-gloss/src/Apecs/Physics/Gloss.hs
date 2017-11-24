{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Physics.Gloss
  ( fromShape, drawWorld, BodyPicture(..),
    ) where

import           Apecs
import           Apecs.Physics
import           Data.Foldable                 (fold)
import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Geometry.Angle (radToDeg)

newtype BodyPicture = BodyPicture G.Picture deriving Monoid
instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

fromShape :: ShapeType -> G.Picture
fromShape (Circle (V2 x y) radius) = G.Translate (realToFrac x) (realToFrac y) $ G.Circle (realToFrac radius)
fromShape (Segment (a,b) _) = G.Line [v2ToTuple a, v2ToTuple b]
fromShape (Convex verts _) = G.Polygon (v2ToTuple <$> verts)

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

drawWorld :: (Has w Physics, Has w BodyPicture) => System w G.Picture
drawWorld = fmap fold . cmapM $ \((Position (V2 x y), Angle theta, BodyPicture pic)) ->
  return . G.Translate (realToFrac x) (realToFrac y) . G.Rotate (negate . radToDeg . realToFrac $ theta) $ pic
