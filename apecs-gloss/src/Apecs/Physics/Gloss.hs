{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Physics.Gloss
  ( convexToPicture
  , drawBody
  , simulate
  , module Apecs.Gloss
  ) where

import           Control.Monad                        (foldM)
import           Data.Semigroup                       (Semigroup (..))
import           Graphics.Gloss.Geometry.Angle        (radToDeg)

import           Apecs
import           Apecs.Physics

import           Apecs.Gloss

newtype PicBuffer = PicBuffer Picture deriving (Semigroup, Monoid)

instance Component PicBuffer where
  type Storage PicBuffer = Map PicBuffer

convexToPicture :: Convex -> Picture
convexToPicture (Convex [V2 x y] radius) = Translate (realToFrac x) (realToFrac y) $ Circle (realToFrac radius)
convexToPicture (Convex [a,b] _) = Line [v2ToTuple a, v2ToTuple b]
convexToPicture (Convex verts _) = Polygon (v2ToTuple <$> verts)

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

drawBody :: Has w IO Shape => (Position, Angle, Shapes) -> System w Picture
drawBody (Position (V2 x y), Angle theta, Shapes shapes) = do
  let fold pic shapeEty = do
        ShapeExtend _ convex <- get shapeEty
        return $ pic <> convexToPicture convex
  bodyPic <- foldM fold mempty shapes
  return . Translate (realToFrac x) (realToFrac y)
         . Rotate (negate . radToDeg . realToFrac $ theta)
         $ bodyPic

simulate
  :: ( Has w IO Camera
     , Has w IO Physics
     )
  => String -> System w ()
simulate name = do
  w <- ask
  play (InWindow name (640,480) (100,100))
       black
       60
       (foldDrawM drawBody)
       (const $ return ())
       (const $ stepPhysics (1/60))
