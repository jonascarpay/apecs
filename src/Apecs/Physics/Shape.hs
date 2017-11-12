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
import qualified Data.IntMap         as M
import           Data.IORef
import           Data.Monoid         ((<>))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Body  ()
import           Apecs.Physics.Space ()
import           Apecs.Physics.Types

C.context phycsCtx
C.include "<chipmunk.h>"

hollowBox :: Double -> Double -> Double -> ShapeProperties -> Shape
hollowBox w h r props = line tl tr <> line tr br <> line br bl <> line bl tl
  where
    line v1 v2 = Shape (Segment v1 v2 r) props
    w' = w/2
    h' = h/2
    tr = V2 w' h'
    tl = V2 (-w') h'
    br = V2 w' (-h')
    bl = V2 (-w') (-h')

toPrimitiveList :: Shape -> [Shape]
toPrimitiveList (Compound shapes) = shapes >>= toPrimitiveList
toPrimitiveList (Shape t p)       = [Shape t p]

instance Component Shape where
  type Storage Shape = Space Shape

instance Has w Physics => Has w Shape where
  getStore = (cast :: Space Physics -> Space Shape) <$> getStore

instance Store (Space Shape) where
  type Stores (Space Shape) = Shape
  type SafeRW (Space Shape) = Shape
  initStore = error "Initializing space from non-Physics store"
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety

  explDestroy (Space eRef _ _) ety = do
    rd <- M.lookup ety <$> readIORef eRef
    case rd of
      Nothing                    -> return ()
      Just (BodyRecord b _ ptrs) -> do
        forM_ ptrs destroyShape
        modifyIORef' eRef (M.insert ety (BodyRecord b mempty []))

  explSetMaybe = explSet
  explSet (Space eRef _ spcPtr) ety sh = do
    rd <- M.lookup ety <$> readIORef eRef
    case rd of
      Nothing -> return ()
      Just (BodyRecord b _ ptrs) -> do
        forM_ ptrs destroyShape
        shPtrs <- forM (toPrimitiveList sh)$ \(Shape shType prop) -> do
          shPtr <- newShape spcPtr b shType ety
          setProperties shPtr prop
          return shPtr
        modifyIORef' eRef (M.insert ety (BodyRecord b sh shPtrs))

  explGetUnsafe = explGet
  explGet (Space eRef _ _) ety = do
    rd <- M.lookup ety <$> readIORef eRef
    return $ case rd of
      Nothing                  -> mempty
      Just (BodyRecord _ sh _) -> sh


maskAll, maskNone :: Bitmask
maskAll  = complement zeroBits
maskNone = zeroBits
maskList :: [Int] -> Bitmask
maskList = foldr (flip setBit) maskNone

defaultProperties :: ShapeProperties
defaultProperties = ShapeProperties False 0 (SMass 1) 0 0 defaultFilter

defaultFilter :: CollisionFilter
defaultFilter = CollisionFilter 0 maskAll maskAll

newShape :: SpacePtr -> Ptr Body -> ShapeType -> Int -> IO (Ptr Shape)
newShape spacePtr' bodyPtr shape (fromIntegral -> ety) = withForeignPtr spacePtr' (go shape)
  where

    go (Circle (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> radius)) spacePtr = [C.block| cpShape* {
      const cpVect vec = { $(double x), $(double y) };
      cpShape* sh = cpCircleShapeNew($(cpBody* bodyPtr), $(double radius), vec);
      cpShapeSetUserData(sh, (void*) $(intptr_t ety));
      return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

    go (Segment (V2 (realToFrac -> xa) (realToFrac -> ya))
                (V2 (realToFrac -> xb) (realToFrac -> yb))
                (realToFrac -> radius)
       ) spacePtr = [C.block| cpShape* {
       const cpVect va = { $(double xa), $(double ya) };
       const cpVect vb = { $(double xb), $(double yb) };
       cpShape* sh = cpSegmentShapeNew($(cpBody* bodyPtr), va, vb, $(double radius));
       return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

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
           cpShape* sh = cpPolyShapeNew($(cpBody* bodyPtr), $(int n), $(cpVect* vecs), trans, $(double radius));
           return cpSpaceAddShape( $(cpSpace* spacePtr), sh); } |]

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
