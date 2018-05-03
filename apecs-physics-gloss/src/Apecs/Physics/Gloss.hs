{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Physics.Gloss
  ( BodyPicture (..), Camera (..),
    toPicture, drawWorld, applyView, mouseToWorld, v2ToTuple,
    defaultSimulate,
  ) where

import           Apecs
import           Apecs.Physics
import           Data.Foldable                        (fold)
import qualified Graphics.Gloss                       as G
import           Graphics.Gloss.Geometry.Angle        (radToDeg)
import qualified Graphics.Gloss.Interface.IO.Simulate as GS

newtype BodyPicture = BodyPicture G.Picture deriving Monoid

instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data Camera = Camera
  { gvOffset :: V2 Double
  , gvScale  :: Double
  }

instance Monoid Camera where
  mempty = Camera 0 1
  mappend (Camera p1 z1) (Camera p2 z2) = Camera (p1 + p2) (z1 * z2)

instance Component Camera where
  type Storage Camera = Global Camera

applyView :: Camera -> G.Picture -> G.Picture
applyView (Camera (V2 x y) scale) =
  G.Scale (realToFrac scale) (realToFrac scale) .  G.Translate (realToFrac . negate $ x) (realToFrac . negate $ y)

mouseToWorld :: (Float,Float) -> Camera -> V2 Double
mouseToWorld (x,y) (Camera offset scale) = (/scale) <$> (V2 (realToFrac x) (realToFrac y))-offset

toPicture :: Convex -> G.Picture
toPicture (Convex [V2 x y] radius) = G.Translate (realToFrac x) (realToFrac y) $ G.Circle (realToFrac radius)
toPicture (Convex [a,b] _) = G.Line [v2ToTuple a, v2ToTuple b]
toPicture (Convex verts _) = G.Polygon (v2ToTuple <$> verts)

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

drawWorld :: (Has w Physics, Has w BodyPicture, Has w Camera) => System w G.Picture
drawWorld = do
  f <- cmapM $ \((Position (V2 x y), Angle theta, BodyPicture pic)) ->
        return . G.Translate (realToFrac x) (realToFrac y) . G.Rotate (negate . radToDeg . realToFrac $ theta) $ pic
  view <- get global
  return . applyView view . fold $ f

defaultSimulate :: (Has w Physics, Has w BodyPicture, Has w Camera) => w -> String -> IO ()
defaultSimulate w name =
  GS.simulateIO (GS.InWindow name (640,480) (100,100)) G.black 60 w render stepper
    where
      render w       = runSystem drawWorld w
      stepper _ dT w = runSystem (stepPhysics (1/60)) w >> return w
