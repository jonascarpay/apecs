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

module Shape where

import           Apecs.Types
import           Control.Monad
import           Data.Bits
import qualified Data.IntMap        as M
import           Data.IORef
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline  as C
import qualified Language.C.Types   as C
import           Linear.V2

import           Instances
import           Types

C.context phycsCtx
C.include "<chipmunk.h>"

instance Component Shapes where
  type Storage Shapes = Space Shapes

instance Has w Body => Has w Shapes where
  getStore = (cast :: Space Body -> Space Shapes) <$> getStore

instance Store (Space Shapes) where
  type Stores (Space Shapes) = Shapes
  type SafeRW (Space Shapes) = Shapes
  initStore = error "Initializing space from non-body store"
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety

  explDestroy (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                    -> return ()
      Just (BodyRecord b _ ptrs) -> do
        forM_ ptrs destroyShape
        modifyIORef' mapRef (M.insert ety (BodyRecord b (Shapes []) []))

  explSet sp@(Space mapRef spcPtr) ety sh@(Shapes shapes) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return ()
      Just (BodyRecord b _ ptrs) -> do
        forM_ ptrs destroyShape
        shPtrs <- forM shapes$ \(Shape shType prop) -> do
          shPtr <- newShape spcPtr b shType
          setProperties shPtr prop
          return shPtr
        modifyIORef' mapRef (M.insert ety (BodyRecord b sh shPtrs))


maskAll, maskNone :: Bitmask
maskAll  = complement zeroBits
maskNone = zeroBits
maskList :: [Int] -> Bitmask
maskList = foldr (flip setBit) maskNone

defaultProperties = ShapeProperties False 0 (SMass 1) 0 0 defaultFilter
defaultFilter     = CollisionFilter 0 maskAll maskAll

newShape :: SpacePtr -> Ptr Body -> ShapeType -> IO (Ptr Shape)
newShape spacePtr' bodyPtr shape = withForeignPtr spacePtr' (go shape)
  where

    go (Circle (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> radius)) spacePtr = [C.block| cpShape* {
      const cpVect vec = { $(double x), $(double y) };
      return cpSpaceAddShape( $(cpSpace* spacePtr)
                            , cpCircleShapeNew($(cpBody* bodyPtr), $(double radius), vec)); } |]

    go (Segment (V2 (realToFrac -> xa) (realToFrac -> ya))
                (V2 (realToFrac -> xb) (realToFrac -> yb))
                (realToFrac -> radius)
       ) spacePtr = [C.block| cpShape* {
       const cpVect va = { $(double xa), $(double ya) };
       const cpVect vb = { $(double xb), $(double yb) };
       return cpSegmentShapeNew($(cpBody* bodyPtr), va, vb, $(double radius)); } |]

    go (Convex ((fmap.fmap) realToFrac -> verts)
               (realToFrac -> radius)
       ) spacePtr = do
         let n = fromIntegral$ length verts
         vecs <- [C.exp| cpVect* { malloc($(int n)*sizeof(cpVect*)) } |]
         forM_ (zip verts [1..]) (\(V2 x y, i) -> [C.block| void {
           $(cpVect* vecs)[$(int i)].x = $(double x);
           $(cpVect* vecs)[$(int i)].y = $(double y); }|])
         [C.block| cpShape* {
           cpTransform trans = { 1, 0, 0, 1, 0, 0 };
           return cpPolyShapeNew($(cpBody* bodyPtr), $(int n), $(cpVect* vecs), trans, $(double radius)); }|]

destroyShape :: Ptr Shape -> IO ()
destroyShape shapePtr = [C.block| void {
  cpShapeDestroy ($(cpShape* shapePtr));
  cpShapeFree ($(cpShape* shapePtr)); }|]

{-data ShapeProperties = ShapeProperties-}
  {-{ sensor          :: Bool-}
  {-, elasticity      :: Double-}
  {-, mass            :: SMass-}
  {-, friction        :: Double-}
  {-, surfaceVelocity :: Vec-}
  {-, group           :: Group-}
  {-, categoryFilter  :: Bitmask-}
  {-, categoryMask    :: Bitmask-}
  {-}-}
  {-deriving (Eq, Show)-}
{-data CollisionFilter-}

getProperties :: Ptr Shape -> IO ShapeProperties
getProperties shape = do
  sensor     <- fromIntegral <$> [C.exp| int          { cpShapeGetSensor($(cpShape* shape))            }|]
  elasticity <- realToFrac   <$> [C.exp| double       { cpShapeGetElasticity($(cpShape* shape))        }|]
  mass       <- realToFrac   <$> [C.exp| double       { cpShapeGetMass($(cpShape* shape))              }|]
  friction   <- realToFrac   <$> [C.exp| double       { cpShapeGetFriction($(cpShape* shape))          }|]
  sx         <- realToFrac   <$> [C.exp| double       { cpShapeGetSurfaceVelocity($(cpShape* shape)).x }|]
  sy         <- realToFrac   <$> [C.exp| double       { cpShapeGetSurfaceVelocity($(cpShape* shape)).y }|]
  group      <-                  [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).group      }|]
  cats       <-                  [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).categories }|]
  mask       <-                  [C.exp| unsigned int { cpShapeGetFilter($(cpShape* shape)).mask       }|]
  return$ ShapeProperties (toEnum sensor) elasticity (SMass mass) friction
                          (V2 sx sy) (CollisionFilter group (Bitmask cats) (Bitmask mask))

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

setProperties :: Ptr Shape -> ShapeProperties -> IO ()
setProperties
  shape
  ( ShapeProperties
      (fromIntegral . fromEnum -> sensor)
      (realToFrac -> elasticity)
      smass
      (realToFrac -> friction)
      (V2 (realToFrac -> sx) (realToFrac -> sy))
      ( CollisionFilter
          group
          (Bitmask cats)
          (Bitmask mask)
      )
  ) = do [C.block| void {
            cpShapeSetSensor($(cpShape* shape), $(int sensor));
            cpShapeSetElasticity($(cpShape* shape), $(double elasticity));
            cpShapeSetFriction($(cpShape* shape), $(double friction));
            const cpVect vec = { $(double sx), $(double sy) };
            cpShapeSetSurfaceVelocity($(cpShape* shape), vec);
            const cpShapeFilter filter = { $(unsigned int group)
                                         , $(unsigned int cats)
                                         , $(unsigned int mask) };
            cpShapeSetFilter($(cpShape* shape), filter); }|]
         case smass of
           SDensity (realToFrac -> density) -> [C.exp| void { cpShapeSetDensity ($(cpShape* shape), $(double density)) } |]
           SMass    (realToFrac -> mass)    -> [C.exp| void { cpShapeSetMass    ($(cpShape* shape), $(double mass))    } |]


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
