{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Apecs.Physics.Render (
  simulateWorld, playWorld,

  Display (..), Color(), makeColor,
  black, white, red, green, blue, cyan, magenta, yellow,

  Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..),
 )where

import           Apecs
import           Apecs.Stores                         (Cache)
import           Data.Foldable
import           Data.Maybe                           (fromMaybe)
import           Graphics.Gloss                       as G
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Interface.IO.Simulate
import           Linear.V2

import           Apecs.Physics.Shape                  as P
import           Apecs.Physics.Space                  as P
import           Apecs.Physics.Types                  as P

toPicture :: Shape -> Picture
toPicture (Shape (P.Circle (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> radius)) _) = translate x y $ Line [(0.25,0),(radius*0.75,0)] `mappend` circle radius
toPicture (Shape (Segment a b _) _) = Line [v2ToTuple a, v2ToTuple b]
toPicture (Shape (Convex verts _) _) = Line (v2ToTuple <$> verts)
toPicture (Compound shapes) = foldMap toPicture shapes

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

drawWorld :: (Has w Physics, Has w Color) => w -> IO Picture
drawWorld w = runWith w . fmap fold . cimapM $ \(ety, (Position (V2 x y), Angle theta, shapes)) -> do
  let pic = foldMap toPicture (toPrimitiveList shapes)
      rotated = rotate (negate . radToDeg . realToFrac $ theta) pic
      translated = translate (realToFrac x) (realToFrac y) rotated
  Safe mcolor :: Safe Color <- get (cast ety)
  return . color (fromMaybe white mcolor) $ translated

simulateWorld :: (Has w Color, Has w Physics) => Display -> Float -> IO w -> System w a -> IO ()
simulateWorld disp scaleFactor initialWorld initializeSys = do
    w <- initialWorld
    runSystem initializeSys w
    simulateIO disp black 60 w render step
  where
    step _ dT w = do
      runSystem (stepPhysics $ realToFrac dT) w
      return w

    render = fmap (scale scaleFactor scaleFactor) . drawWorld

playWorld :: (Has w Color, Has w Physics)
          => Display
          -> Float
          -> IO w
          -> System w a
          -> (Event -> System w ())
          -> System w b
          -> IO ()
playWorld disp scaleFactor initialWorld initializeSys eventSys stepSys = do
    w <- initialWorld
    runSystem initializeSys w
    playIO disp black 60 w render handler step
  where
    step dT w = do
      runSystem (stepPhysics (realToFrac dT) >> stepSys) w
      return w
    render = fmap (scale scaleFactor scaleFactor) . drawWorld
    handler event w = do
      runSystem (eventSys event) w
      return w

instance Component Color where
  type Storage Color = Cache 100 (Map Color)
