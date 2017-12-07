{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Physics.Gloss
  ( BodyPicture (..), GlossView (..),
    fromShape, drawWorld, applyView, mouseToWorld, v2ToTuple,
    defaultSimulate,
  ) where

import           Apecs
import           Apecs.Physics
import           Data.Foldable                        (fold)
import qualified Graphics.Gloss                       as G
import           Graphics.Gloss.Geometry.Angle        (radToDeg)
import qualified Graphics.Gloss.Interface.IO.Simulate as G

newtype BodyPicture = BodyPicture G.Picture deriving Monoid

instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data GlossView = GlossView
  { gvOffset :: V2 Double
  , gvScale  :: Double
  }

instance Monoid GlossView where mempty = GlossView 0 1

instance Component GlossView where
  type Storage GlossView = Global GlossView

applyView :: GlossView -> G.Picture -> G.Picture
applyView (GlossView (V2 x y) scale) =
  G.Scale (realToFrac scale) (realToFrac scale) .  G.Translate (realToFrac . negate $ x) (realToFrac . negate $ y)

mouseToWorld :: (Float,Float) -> GlossView -> V2 Double
mouseToWorld (x,y) (GlossView offset scale) = (/scale) <$> (V2 (realToFrac x) (realToFrac y))-offset

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

defaultSimulate :: (Has w Physics, Has w BodyPicture, Has w GlossView) => w -> IO ()
defaultSimulate w =
  G.simulateIO (G.InWindow "Tumbler" (640,480) (100,100)) G.black 60 w render stepper
    where
      render w       = runSystem drawWorld w
      stepper _ dT w = runSystem (stepPhysics (1/60)) w >> return w
