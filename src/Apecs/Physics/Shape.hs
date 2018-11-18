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

import           Apecs.Core
import           Control.Monad
import           Data.Bits
import qualified Data.IntMap          as M
import qualified Data.IntSet          as S
import           Data.IORef
import           Data.Monoid          ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed  as U
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline    as C
import           Linear.V2

import           Apecs.Physics.Space  ()
import           Apecs.Physics.Types

C.context (phycsCtx <> C.vecCtx)
C.include "<chipmunk.h>"

maskAll, maskNone :: Bitmask
maskAll  = complement zeroBits
maskNone = zeroBits
maskList :: [Int] -> Bitmask
maskList = foldr (flip setBit) maskNone

defaultFilter :: CollisionFilter
defaultFilter = CollisionFilter 0 maskAll maskAll

boxShape :: Double -> Double -> Vec -> Convex
boxShape w h offset = Convex ((+offset) <$> verts) 0
  where
    w' = w/2
    h' = h/2
    verts = [ V2 (-w') (-h')
            , V2 (-w') h'
            , V2 w' h'
            , V2 w' (-h') ]

instance Component Shape where
  type Storage Shape = Space Shape

instance Has w IO Physics => Has w IO Shape where
  getStore = (cast :: Space Physics -> Space Shape) <$> getStore

instance ExplMembers IO (Space Shape) where
  explMembers (Space _ sMap _ _ _) = U.fromList . M.keys <$> readIORef sMap

instance ExplDestroy IO (Space Shape) where
  explDestroy (Space bMap sMap _ _ spc) sEty = do
    rd <- M.lookup sEty <$> readIORef sMap
    forM_ rd $ \sPtr -> do
      bEty <- fromIntegral <$> getShapeBody sPtr

      Just bRec <- M.lookup bEty <$> readIORef bMap
      let brShapes' = S.delete sEty (brShapes bRec)

      modifyIORef' sMap (M.delete sEty)
      modifyIORef' bMap (M.insert bEty (bRec {brShapes = brShapes'}))
      destroyShape spc sPtr

instance ExplSet IO (Space Shape) where
  explSet _ _ ShapeRead = return ()
  explSet sp ety (Shape sh) = explSet sp ety (ShapeExtend (Entity ety) sh)

  explSet sp@(Space bMap sMap _ _ spcPtr) sEty (ShapeExtend (Entity bEty) sh) = do
    explDestroy sp sEty
    rd <- M.lookup bEty <$> readIORef bMap
    forM_ rd $ \bRec -> do
      s <- newShape spcPtr (brPtr bRec) sh sEty
      let brShapes' = S.insert sEty (brShapes bRec)
      modifyIORef' bMap (M.insert bEty (bRec {brShapes = brShapes'}))
      modifyIORef' sMap (M.insert sEty s)

instance ExplGet IO (Space Shape) where
  explGet _ _ = return ShapeRead
  explExists (Space _ sMap _ _ _) ety = M.member ety <$> readIORef sMap

newShape :: SpacePtr -> Ptr Body -> Convex -> Int -> IO (Ptr Shape)
newShape spacePtr' bodyPtr shape (fromIntegral -> ety) = withForeignPtr spacePtr' (go shape)
  where

    go (Convex [fmap realToFrac -> V2 x y] (realToFrac -> radius)) spacePtr = [C.block| cpShape* {
      const cpVect vec = { $(double x), $(double y) };
      cpShape* sh = cpCircleShapeNew($(cpBody* bodyPtr), $(double radius), vec);
      cpShapeSetUserData(sh, (void*) $(intptr_t ety));
      return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

    go (Convex [ fmap realToFrac -> V2 xa ya
               , fmap realToFrac -> V2 xb yb ]
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

destroyShape :: SpacePtr -> Ptr Shape -> IO ()
destroyShape spacePtr shapePtr = withForeignPtr spacePtr $ \space -> [C.block| void {
  cpShape *shape = $(cpShape* shapePtr);
  cpSpaceRemoveShape($(cpSpace* space), shape);
  cpShapeFree (shape); }|]

-- Sensor
getSensor :: Ptr Shape -> IO Bool
getSensor shape = toEnum . fromIntegral <$> [C.exp| int {
  cpShapeGetSensor($(cpShape* shape)) }|]

setSensor :: Ptr Shape -> Bool -> IO ()
setSensor shape (fromIntegral . fromEnum -> isSensor) = [C.exp| void {
  cpShapeSetSensor($(cpShape* shape), $(int isSensor)) }|]

instance Component Sensor where
  type Storage Sensor = Space Sensor
instance Has w IO Physics => Has w IO Sensor where
  getStore = (cast :: Space Physics -> Space Sensor) <$> getStore

instance ExplMembers IO (Space Sensor) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space Sensor) where
  explSet (Space _ sMap _ _ _) ety (Sensor vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setSensor s vec

instance ExplGet IO (Space Sensor) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    Sensor <$> getSensor s

-- Elasticity
getElasticity :: Ptr Shape -> IO Double
getElasticity shape = realToFrac <$> [C.exp| double {
  cpShapeGetElasticity($(cpShape* shape)) }|]

setElasticity :: Ptr Shape -> Double -> IO ()
setElasticity shape (realToFrac -> elasticity) = [C.exp| void {
  cpShapeSetElasticity($(cpShape* shape), $(double elasticity)) }|]

instance Component Elasticity where
  type Storage Elasticity = Space Elasticity
instance Has w IO Physics => Has w IO Elasticity where
  getStore = (cast :: Space Physics -> Space Elasticity) <$> getStore

instance ExplMembers IO (Space Elasticity) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space Elasticity) where
  explSet (Space _ sMap _ _ _) ety (Elasticity vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setElasticity s vec

instance ExplGet IO (Space Elasticity) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    Elasticity <$> getElasticity s

-- Mass
getMass :: Ptr Shape -> IO Double
getMass shape = realToFrac <$> [C.exp| double {
  cpShapeGetMass($(cpShape* shape)) }|]

setMass :: Ptr Shape -> Double -> IO ()
setMass shape (realToFrac -> mass) = [C.exp| void {
  cpShapeSetMass($(cpShape* shape), $(double mass)) }|]

instance Component Mass where
  type Storage Mass = Space Mass
instance Has w IO Physics => Has w IO Mass where
  getStore = (cast :: Space Physics -> Space Mass) <$> getStore

instance ExplMembers IO (Space Mass) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space Mass) where
  explSet (Space _ sMap _ _ _) ety (Mass vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setMass s vec

instance ExplGet IO (Space Mass) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    Mass <$> getMass s

-- Density
getDensity :: Ptr Shape -> IO Double
getDensity shape = realToFrac <$> [C.exp| double {
  cpShapeGetDensity($(cpShape* shape)) }|]

setDensity :: Ptr Shape -> Double -> IO ()
setDensity shape (realToFrac -> density) = [C.exp| void {
  cpShapeSetDensity($(cpShape* shape), $(double density)) }|]

instance Component Density where
  type Storage Density = Space Density
instance Has w IO Physics => Has w IO Density where
  getStore = (cast :: Space Physics -> Space Density) <$> getStore

instance ExplMembers IO (Space Density) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space Density) where
  explSet (Space _ sMap _ _ _) ety (Density vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setDensity s vec

instance ExplGet IO (Space Density) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    Density <$> getDensity s

-- Friction
getFriction :: Ptr Shape -> IO Double
getFriction shape = realToFrac <$> [C.exp| double {
  cpShapeGetFriction($(cpShape* shape)) }|]

setFriction :: Ptr Shape -> Double -> IO ()
setFriction shape (realToFrac -> friction) = [C.exp| void {
  cpShapeSetFriction($(cpShape* shape), $(double friction)) }|]

instance Component Friction where
  type Storage Friction = Space Friction
instance Has w IO Physics => Has w IO Friction where
  getStore = (cast :: Space Physics -> Space Friction) <$> getStore

instance ExplMembers IO (Space Friction) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space Friction) where
  explSet (Space _ sMap _ _ _) ety (Friction vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setFriction s vec

instance ExplGet IO (Space Friction) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    Friction <$> getFriction s

-- SurfaceVelocity
getSurfaceVelocity :: Ptr Shape -> IO Vec
getSurfaceVelocity shape = do
 x <- [C.exp| double { cpShapeGetSurfaceVelocity($(cpShape* shape)).x }|]
 y <- [C.exp| double { cpShapeGetSurfaceVelocity($(cpShape* shape)).y }|]
 return (V2 (realToFrac x) (realToFrac y))

setSurfaceVelocity :: Ptr Shape -> Vec -> IO ()
setSurfaceVelocity shape (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpShapeSetSurfaceVelocity($(cpShape* shape), vec);
  }|]

instance Component SurfaceVelocity where
  type Storage SurfaceVelocity = Space SurfaceVelocity
instance Has w IO Physics => Has w IO SurfaceVelocity where
  getStore = (cast :: Space Physics -> Space SurfaceVelocity) <$> getStore

instance ExplMembers IO (Space SurfaceVelocity) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space SurfaceVelocity) where
  explSet (Space _ sMap _ _ _) ety (SurfaceVelocity vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setSurfaceVelocity s vec

instance ExplGet IO (Space SurfaceVelocity) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    SurfaceVelocity <$> getSurfaceVelocity s

-- CollisionFilter
getFilter :: Ptr Shape -> IO CollisionFilter
getFilter shape = do
 group <- [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).group }|]
 cats  <- [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).categories }|]
 mask  <- [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).mask }|]
 return$ CollisionFilter group (Bitmask cats) (Bitmask mask)

setFilter :: Ptr Shape -> CollisionFilter -> IO ()
setFilter shape (CollisionFilter group (Bitmask cats) (Bitmask mask)) =
  [C.block| void {
    const cpShapeFilter filter = { $(unsigned int group)
                                 , $(unsigned int cats)
                                 , $(unsigned int mask) };
    cpShapeSetFilter($(cpShape* shape), filter);
  }|]

instance Component CollisionFilter where
  type Storage CollisionFilter = Space CollisionFilter
instance Has w IO Physics => Has w IO CollisionFilter where
  getStore = (cast :: Space Physics -> Space CollisionFilter) <$> getStore

instance ExplMembers IO (Space CollisionFilter) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space CollisionFilter) where
  explSet (Space _ sMap _ _ _) ety cfilter = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setFilter s cfilter

instance ExplGet IO (Space CollisionFilter) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    getFilter s

-- CollisionType
getCollisionType :: Ptr Shape -> IO C.CUIntPtr
getCollisionType shape = [C.exp| uintptr_t {
  cpShapeGetCollisionType($(cpShape* shape)) }|]

setCollisionType :: Ptr Shape -> C.CUIntPtr -> IO ()
setCollisionType shape ctype = [C.exp| void {
  cpShapeSetCollisionType($(cpShape* shape), $(uintptr_t ctype)) }|]

instance Component CollisionType where
  type Storage CollisionType = Space CollisionType
instance Has w IO Physics => Has w IO CollisionType where
  getStore = (cast :: Space Physics -> Space CollisionType) <$> getStore

instance ExplMembers IO (Space CollisionType) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplSet IO (Space CollisionType) where
  explSet (Space _ sMap _ _ _) ety (CollisionType vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setCollisionType s vec

instance ExplGet IO (Space CollisionType) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    CollisionType <$> getCollisionType s

-- ShapeBody
getShapeBody :: Ptr Shape -> IO C.CUIntPtr
getShapeBody shape = [C.exp| uintptr_t {
  (intptr_t) cpBodyGetUserData(cpShapeGetBody($(cpShape* shape))) }|]

instance Component ShapeBody where
  type Storage ShapeBody = Space ShapeBody
instance Has w IO Physics => Has w IO ShapeBody where
  getStore = (cast :: Space Physics -> Space ShapeBody) <$> getStore

instance ExplMembers IO (Space ShapeBody) where
  explMembers s = explMembers (cast s :: Space Shape)

instance ExplGet IO (Space ShapeBody) where
  explExists s ety = explExists (cast s :: Space Shape) ety
  explGet (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    ShapeBody . Entity . fromIntegral <$> getShapeBody s
