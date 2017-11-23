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
import qualified Data.IntMap          as M
import           Data.IORef
import           Data.Monoid          ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed  as U
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline    as C
import           Linear.V2

import           Apecs.Physics.Body   ()
import           Apecs.Physics.Space  ()
import           Apecs.Physics.Types
import           Apecs.Stores         (defaultSetMaybe)

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
  explMembers (Space _ sMap _ _ _) = U.fromList . M.keys <$> readIORef sMap
  explExists (Space _ sMap _ _ _) ety = M.member ety <$> readIORef sMap

  explDestroy (Space _ sMap _ _ spc) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd $ \ s -> do
      destroyShape spc s
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
instance Has w Physics => Has w Sensor where
  getStore = (cast :: Space Physics -> Space Sensor) <$> getStore

instance Store (Space Sensor) where
  type Stores (Space Sensor) = Sensor
  type SafeRW (Space Sensor) = Maybe Sensor
  initStore = error "Attempted to initialize a space from an Sensor component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . Sensor <$> getSensor s

  explSet (Space _ sMap _ _ _) ety (Sensor vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setSensor s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w Elasticity where
  getStore = (cast :: Space Physics -> Space Elasticity) <$> getStore

instance Store (Space Elasticity) where
  type Stores (Space Elasticity) = Elasticity
  type SafeRW (Space Elasticity) = Maybe Elasticity
  initStore = error "Attempted to initialize a space from an Elasticity component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . Elasticity <$> getElasticity s

  explSet (Space _ sMap _ _ _) ety (Elasticity vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setElasticity s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w Mass where
  getStore = (cast :: Space Physics -> Space Mass) <$> getStore

instance Store (Space Mass) where
  type Stores (Space Mass) = Mass
  type SafeRW (Space Mass) = Maybe Mass
  initStore = error "Attempted to initialize a space from an Mass component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . Mass <$> getMass s

  explSet (Space _ sMap _ _ _) ety (Mass vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setMass s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w Density where
  getStore = (cast :: Space Physics -> Space Density) <$> getStore

instance Store (Space Density) where
  type Stores (Space Density) = Density
  type SafeRW (Space Density) = Maybe Density
  initStore = error "Attempted to initialize a space from an Density component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . Density <$> getDensity s

  explSet (Space _ sMap _ _ _) ety (Density vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setDensity s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w Friction where
  getStore = (cast :: Space Physics -> Space Friction) <$> getStore

instance Store (Space Friction) where
  type Stores (Space Friction) = Friction
  type SafeRW (Space Friction) = Maybe Friction
  initStore = error "Attempted to initialize a space from an Friction component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . Friction <$> getFriction s

  explSet (Space _ sMap _ _ _) ety (Friction vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setFriction s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w SurfaceVelocity where
  getStore = (cast :: Space Physics -> Space SurfaceVelocity) <$> getStore

instance Store (Space SurfaceVelocity) where
  type Stores (Space SurfaceVelocity) = SurfaceVelocity
  type SafeRW (Space SurfaceVelocity) = Maybe SurfaceVelocity
  initStore = error "Attempted to initialize a space from an SurfaceVelocity component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . SurfaceVelocity <$> getSurfaceVelocity s

  explSet (Space _ sMap _ _ _) ety (SurfaceVelocity vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setSurfaceVelocity s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w CollisionFilter where
  getStore = (cast :: Space Physics -> Space CollisionFilter) <$> getStore

instance Store (Space CollisionFilter) where
  type Stores (Space CollisionFilter) = CollisionFilter
  type SafeRW (Space CollisionFilter) = Maybe CollisionFilter
  initStore = error "Attempted to initialize a space from an CollisionFilter component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just <$> getFilter s

  explSet (Space _ sMap _ _ _) ety cfilter = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setFilter s cfilter

  explGetUnsafe (Space _ sMap _ _ _) ety = do
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
instance Has w Physics => Has w CollisionType where
  getStore = (cast :: Space Physics -> Space CollisionType) <$> getStore

instance Store (Space CollisionType) where
  type Stores (Space CollisionType) = CollisionType
  type SafeRW (Space CollisionType) = Maybe CollisionType
  initStore = error "Attempted to initialize a space from an CollisionType component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Shape)
  explExists s ety = explExists (cast s :: Space Shape) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ sMap _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef sMap
    case rd of
      Nothing -> return Nothing
      Just s  -> Just . CollisionType <$> getCollisionType s

  explSet (Space _ sMap _ _ _) ety (CollisionType vec) = do
    rd <- M.lookup ety <$> readIORef sMap
    forM_ rd$ \s -> setCollisionType s vec

  explGetUnsafe (Space _ sMap _ _ _) ety = do
    Just s <- M.lookup ety <$> readIORef sMap
    CollisionType <$> getCollisionType s

