{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Render where

import           Apecs
import           Apecs.Types
import           Data.Foldable
import           Graphics.Gloss                       as G
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Interface.IO.Simulate
import           Linear.V2

import           Instances                            as P
import           Shape                                as P
import           Types                                as P
import           Wrapper                              as P

toPicture :: Shape -> Picture
toPicture (Shape (P.Circle (V2 x y) radius) _) = translate (realToFrac x) (realToFrac y) $ circle (realToFrac radius)
toPicture (Shape (Segment a b radius) _) = undefined
toPicture (Shape (Convex verts radius) _) = undefined

drawWorld :: Has w Physics => w -> IO Picture
drawWorld w = runWith w . fmap fold . cmapM $ \(Position (V2 x y), Angle theta, Shapes sh) -> do
  let pic = foldMap toPicture sh
      rotated = rotate (radToDeg . realToFrac $ theta) pic
      translated = translate (realToFrac x) (realToFrac y) rotated
  return . scale 100 100 . color white $ translated

simulateWorld :: Has w Physics => Display -> IO w -> System w () -> IO ()
simulateWorld disp initialWorld intializeSys = do
    w <- initialWorld
    runSystem intializeSys w
    simulateIO disp black 30 w drawWorld stepSys
  where
    stepSys viewport dT w = do
      runSystem (stepPhysicsSys $ realToFrac dT) w
      return w

