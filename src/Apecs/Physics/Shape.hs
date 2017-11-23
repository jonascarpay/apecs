{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Apecs.Physics.Shape where

import           Apecs.Types
import           Control.Monad
import           Data.Bits
import qualified Data.IntMap                  as M
import           Data.IORef
import           Data.Monoid                  ((<>))
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc        (free, malloc)
import           Foreign.Ptr
import qualified Language.C.Inline            as C
import           Linear.V2

import           Apecs.Physics.Body           ()
import           Apecs.Physics.Space          ()
import           Apecs.Physics.Types
import           Apecs.Stores                 (defaultSetMaybe)

C.context (phycsCtx <> C.vecCtx)
C.include "<chipmunk.h>"

instance Component Shape where
  type Storage Shape = Space Shape

instance Has w Physics => Has w Shape where
  getStore = (cast :: Space Physics -> Space Shape) <$> getStore

instance Store (Space Shape) where
  type Stores (Space Shape) = Shape
  type SafeRW (Space Shape) = Maybe Shape
  initStore = error "Initializing space from non-Physics store"
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety

  explDestroy (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd $ \ s -> do
      destroyShape s
      modifyIORef' sMap (M.delete ety)

  explSetMaybe = defaultSetMaybe
  explSet _ _ ShapeRead = return ()
  explSet sp ety (Shape sh) = explSet sp ety (ShapeExtend (Entity ety) sh)
  explSet sp@(Space bMap sMap _ _ spcPtr) ety (ShapeExtend (Entity bEty) sh) = do
    rd <- M.lookup bEty <$> readIORef bMap
    forM_ rd $ \b -> do
      explDestroy sp ety
      s <- newShape spcPtr b sh ety
      modifyIORef' sMap (M.insert ety s)

  explGet s ety = do
    e <- explExists s ety
    if e then Just <$> explGetUnsafe s ety else return Nothing
  explGetUnsafe _ _ = return (error "Shape is a read-only component")

maskAll, maskNone :: Bitmask
maskAll  = complement zeroBits
maskNone = zeroBits
maskList :: [Int] -> Bitmask
maskList = foldr (flip setBit) maskNone

newShape :: SpacePtr -> Ptr Body -> ShapeType -> Int -> IO (Ptr Shape)
newShape spacePtr' bodyPtr shape (fromIntegral -> ety) = withForeignPtr spacePtr' (go shape)
  where

    go (Circle (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> radius)) spacePtr = [C.block| cpShape* {
      const cpVect vec = { $(double x), $(double y) };
      cpShape* sh = cpCircleShapeNew($(cpBody* bodyPtr), $(double radius), vec);
      cpShapeSetUserData(sh, (void*) $(intptr_t ety));
      return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

    go (Segment (V2 (realToFrac -> xa) (realToFrac -> ya),
                 V2 (realToFrac -> xb) (realToFrac -> yb))
                (realToFrac -> radius)
       ) spacePtr = [C.block| cpShape* {
       const cpVect va = { $(double xa), $(double ya) };
       const cpVect vb = { $(double xb), $(double yb) };
       cpShape* sh = cpSegmentShapeNew($(cpBody* bodyPtr), va, vb, $(double radius));
       cpShapeSetUserData(sh, (void*) $(intptr_t ety));
       return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

    go (Convex ((fmap.fmap) realToFrac -> verts)
               (realToFrac -> radius)
       ) spacePtr = do
         vec <- V.thaw (V.fromList verts)
         [C.block| cpShape* {
           cpTransform trans = cpTransformIdentity;
           cpShape* sh = cpPolyShapeNew($(cpBody* bodyPtr), $vec-len:vec, $vec-ptr:(cpVect *vec), trans, $(double radius));
           cpShapeSetUserData(sh, (void*) $(intptr_t ety));
           return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

destroyShape :: Ptr Shape -> IO ()
destroyShape shapePtr = [C.block| void {
  cpShapeDestroy ($(cpShape* shapePtr));
  cpShapeFree ($(cpShape* shapePtr)); }|]

getSensor :: Ptr Shape -> IO Bool
getSensor shape = toEnum . fromIntegral <$> [C.exp| int {
  cpShapeGetSensor($(cpShape* shape)) }|]

getElasticity :: Ptr Shape -> IO Double
getElasticity shape = realToFrac <$> [C.exp| double {
  cpShapeGetElasticity($(cpShape* shape)) }|]

getMass :: Ptr Shape -> IO Double
getMass shape = realToFrac <$> [C.exp| double {
  cpShapeGetMass($(cpShape* shape)) }|]

getDensity :: Ptr Shape -> IO Double
getDensity shape = realToFrac <$> [C.exp| double {
  cpShapeGetDensity($(cpShape* shape)) }|]

getFriction :: Ptr Shape -> IO Double
getFriction shape = realToFrac <$> [C.exp| double {
  cpShapeGetFriction($(cpShape* shape)) }|]

getSurfaceVelocity :: Ptr Shape -> IO Vec
getSurfaceVelocity shape = do
 x <- [C.exp| double { cpShapeGetSurfaceVelocity($(cpShape* shape)).x }|]
 y <- [C.exp| double { cpShapeGetSurfaceVelocity($(cpShape* shape)).y }|]
 return (V2 (realToFrac x) (realToFrac y))

getFilter :: Ptr Shape -> IO CollisionFilter
getFilter shape = do
 group <- [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).group }|]
 cats  <- [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).categories }|]
 mask  <- [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).mask }|]
 return$ CollisionFilter group (Bitmask cats) (Bitmask mask)

setSensor :: Ptr Shape -> Bool -> IO ()
setSensor shape (fromIntegral . fromEnum -> isSensor) = [C.exp| void {
  cpShapeSetSensor($(cpShape* shape), $(int isSensor)) }|]

setElasticity :: Ptr Shape -> Double -> IO ()
setElasticity shape (realToFrac -> elasticity) = [C.exp| void {
  cpShapeSetElasticity($(cpShape* shape), $(double elasticity)) }|]

setMass :: Ptr Shape -> Double -> IO ()
setMass shape (realToFrac -> mass) = [C.exp| void {
  cpShapeSetMass($(cpShape* shape), $(double mass)) }|]

setDensity :: Ptr Shape -> Double -> IO ()
setDensity shape (realToFrac -> density) = [C.exp| void {
  cpShapeSetDensity($(cpShape* shape), $(double density)) }|]

setFriction :: Ptr Shape -> Double -> IO ()
setFriction shape (realToFrac -> friction) = [C.exp| void {
  cpShapeSetFriction($(cpShape* shape), $(double friction)) }|]

setSurfaceVelocty :: Ptr Shape -> Vec -> IO ()
setSurfaceVelocty shape (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpShapeSetSurfaceVelocity($(cpShape* shape), vec);
  }|]

setFilter :: Ptr Shape -> CollisionFilter -> IO ()
setFilter shape (CollisionFilter group (Bitmask cats) (Bitmask mask)) =
  [C.block| void {
    const cpShapeFilter filter = { $(unsigned int group)
                                 , $(unsigned int cats)
                                 , $(unsigned int mask) };
    cpShapeSetFilter($(cpShape* shape), filter);
  }|]
