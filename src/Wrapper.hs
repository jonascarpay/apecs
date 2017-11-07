{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Wrapper where

import Linear.V2
import Apecs
import qualified Data.Vector.Unboxed as U
import Apecs.Types
import Apecs.TH
import Data.Monoid ((<>))
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.IORef
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import Control.Monad

import Context

C.context phycsCtx
C.include "<chipmunk.h>"

-- Space
newSpace :: IO SpacePtr
newSpace = do
    spaceRaw <- [C.exp| cpSpace* { cpSpaceNew() } |]
    newForeignPtr spaceRaw $ do
      error "deallocate other objects before removing the space"
      [C.exp| void { cpSpaceFree($(cpSpace* spaceRaw)) } |] -- FIXME: deallocate all map entries

stepPhysics :: SpacePtr -> Double -> IO ()
stepPhysics spacePtr (realToFrac -> dT) = withForeignPtr spacePtr $ \space ->
  [C.exp| void { cpSpaceStep( $(cpSpace* space), $(double dT) ) } |]

-- Body
newBody :: SpacePtr -> IO (Ptr Body)
newBody spacePtr = do
  withForeignPtr spacePtr $ \space -> [C.block| cpBody* {
    cpBody* body = cpBodyNew(0,0);
    cpSpaceAddBody($(cpSpace* space), body);
    cpShape* shape = cpSpaceAddShape($(cpSpace* space), cpCircleShapeNew(body, 1, cpvzero));
    return body; } |]

setBodyType :: Ptr Body -> Body -> IO ()
setBodyType bodyPtr (fromIntegral . fromEnum -> bodyInt) =
  [C.exp| void { cpBodySetType($(cpBody* bodyPtr), $(int bodyInt)) } |]

getBodyType :: Ptr Body -> IO Body
getBodyType bodyPtr = toEnum . fromIntegral <$> [C.exp| int { cpBodyGetType($(cpBody* bodyPtr)) } |]

destroyBody :: Ptr Body -> IO ()
destroyBody  bodyPtr  = [C.block| void { cpBodyDestroy ($(cpBody*  bodyPtr));  cpBodyFree ($(cpBody*  bodyPtr));  } |]

-- Position
getPosition :: Ptr Body -> IO (V2 Double)
getPosition bodyPtr = do
  x <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setPosition :: Ptr Body -> V2 Double -> IO ()
setPosition bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpBodySetPosition($(cpBody* bodyPtr), vec);
  } |]

-- Gravity
getGravity :: SpacePtr -> IO (V2 Double)
getGravity spacePtr = withForeignPtr spacePtr $ \space -> do
  x <- [C.exp| double { cpSpaceGetGravity ($(cpSpace* space)).x } |]
  y <- [C.exp| double { cpSpaceGetGravity ($(cpSpace* space)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setGravity :: SpacePtr -> V2 Double -> IO ()
setGravity spacePtr (V2 (realToFrac -> x) (realToFrac -> y)) = withForeignPtr spacePtr $ \space -> [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpSpaceSetGravity($(cpSpace* space), vec);
  } |]

-- Mass
getMass :: Ptr Body -> IO Double
getMass bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMass ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMass :: Ptr Body -> Double -> IO ()
setMass bodyPtr (realToFrac -> mass) = [C.exp| void { cpBodySetMass($(cpBody* bodyPtr), $(double mass)); } |]

-- Moment
getMoment :: Ptr Body -> IO Double
getMoment bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMoment ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMoment :: Ptr Body -> Double -> IO ()
setMoment bodyPtr (realToFrac -> moment) = [C.exp| void { cpBodySetMoment($(cpBody* bodyPtr), $(double moment)); } |]

-- Shape
newShape :: SpacePtr -> Ptr Body -> ShapeType -> IO (Ptr Shape)
newShape spacePtr' bodyPtr shape = withForeignPtr spacePtr' (go shape)
  where

    go (Circle (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> radius)) spacePtr = do [C.block| cpShape* {
      const cpVect vec = { $(double x), $(double y) };
      return cpCircleShapeNew($(cpBody* bodyPtr), $(double radius), vec); } |]

    go (Segment (V2 (realToFrac -> xa) (realToFrac -> ya))
                (V2 (realToFrac -> xb) (realToFrac -> yb))
                (realToFrac -> radius)
       ) spacePtr = do [C.block| cpShape* {
       const cpVect va = { $(double xa), $(double ya) };
       const cpVect vb = { $(double xb), $(double yb) };
       return cpSegmentShapeNew($(cpBody* bodyPtr), va, vb, $(double radius)); } |]

    go (Convex ((fmap.fmap) realToFrac -> verts)
               (realToFrac -> radius)
       ) spacePtr =
         do let n = fromIntegral$ length verts
            vecs <- [C.exp| cpVect* { malloc($(int n)*sizeof(cpVect*)) } |]
            forM_ (zip verts [1..]) (\((V2 x y), i) -> [C.block| void {
                $(cpVect* vecs)[$(int i)].x = $(double x);
                $(cpVect* vecs)[$(int i)].y = $(double y);
              }|])

            [C.block| cpShape* {
              cpTransform trans = { 1, 0, 0, 1, 0, 0 };
              return cpPolyShapeNew($(cpBody* bodyPtr), $(int n), $(cpVect* vecs), trans, $(double radius));
            }|]

destroyShape :: Ptr Shape -> IO ()
destroyShape shapePtr = [C.block| void {
  cpShapeDestroy ($(cpShape* shapePtr));
  cpShapeFree ($(cpShape* shapePtr)); }|]

