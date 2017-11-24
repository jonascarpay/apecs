{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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

data GlossView = GlossView (V2 Double) Double
instance Monoid GlossView where mempty = GlossView 0 1
instance Component GlossView where
  type Storage GlossView = Global GlossView

applyView :: GlossView -> G.Picture -> G.Picture
applyView (GlossView (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> scale)) =
  G.Translate x y . G.Scale scale scale

mouseToWorld :: GlossView -> V2 Double -> V2 Double
mouseToWorld (GlossView offset scale) mvec = (/scale) <$> mvec-offset

fromShape :: ShapeType -> G.Picture
fromShape (Circle (V2 x y) radius) = G.Translate (realToFrac x) (realToFrac y) $ G.Circle (realToFrac radius)
fromShape (Segment (a,b) _) = G.Line [v2ToTuple a, v2ToTuple b]
fromShape (Convex verts _) = G.Polygon (v2ToTuple <$> verts)

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

drawWorld :: (Has w Physics, Has w BodyPicture, Has w GlossView) => System w G.Picture
drawWorld = do
  f <- cmapM $ \((Position (V2 x y), Angle theta, BodyPicture pic)) ->
        return . G.Translate (realToFrac x) (realToFrac y) . G.Rotate (negate . radToDeg . realToFrac $ theta) $ pic
  view <- getGlobal
  return . applyView view . fold $ f
