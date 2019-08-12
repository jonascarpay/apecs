{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Physics.Gloss
  ( convexToPicture
  , Transform, worldTransform, drawBody
  , simulate
  , module Apecs.Gloss
  ) where

import           Control.Monad                        (foldM)
import           Data.Semigroup                       (Semigroup (..))
import           Graphics.Gloss.Geometry.Angle        (radToDeg)
import           Graphics.Gloss.Interface.IO.Simulate (simulateIO)

import           Apecs
import           Apecs.Physics

import           Apecs.Gloss

convexToPicture :: Convex -> Picture
convexToPicture (Convex [V2 x y] radius) = Translate (realToFrac x) (realToFrac y) $ Circle (realToFrac radius)
convexToPicture (Convex [a,b] _) = Line [v2ToTuple a, v2ToTuple b]
convexToPicture (Convex verts _) = Polygon (v2ToTuple <$> verts)

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

type Transform = (Position, Angle)
worldTransform :: Transform -> Picture -> Picture
worldTransform (Position (V2 x y), Angle theta) =
 Translate (realToFrac x) (realToFrac y) .
 Rotate (negate . radToDeg . realToFrac $ theta)

drawBody :: Has w IO Physics => (Body, Transform, ShapeList) -> System w Picture
drawBody (btype, transform, ShapeList shapes) = color shColor . worldTransform transform <$> foldM foldfn mempty shapes
  where foldfn pic shapeEty = do
          Shape _ convex <- get shapeEty
          return . mappend pic $ convexToPicture convex
        shColor = case btype of
                    DynamicBody   -> red
                    KinematicBody -> green
                    StaticBody    -> blue

simulate
  :: ( Has w IO Camera
     , Has w IO Physics
     )
  => Display
  -> System w ()
simulate disp = do
  w <- ask
  liftIO $
     simulateIO disp
                black
                60
                w
                (\_     -> runSystem draw w)
                (\_ _ _ -> runSystem (stepPhysics (1/60)) w >> return w)

  where
    draw = do
      pic <- foldDrawM drawBody
      cam <- get global
      return $ cameraTransform cam pic
