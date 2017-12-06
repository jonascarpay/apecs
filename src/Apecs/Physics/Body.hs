{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Apecs.Physics.Body where

import           Apecs
import           Apecs.Stores        (defaultSetMaybe)
import           Apecs.Types
import           Control.Monad
import qualified Data.IntMap         as M
import qualified Data.IntSet         as S
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Space ()
import           Apecs.Physics.Shape ()
import           Apecs.Physics.Constraint ()
import           Apecs.Physics.Types

C.context phycsCtx
C.include "<chipmunk.h>"

-- Body
newBody :: SpacePtr -> Int -> IO (Ptr Body)
newBody spacePtr (fromIntegral -> ety) = withForeignPtr spacePtr $ \space -> [C.block| cpBody* {
    cpBody* body = cpBodyNew(0,0);
    cpSpaceAddBody($(cpSpace* space), body);
    cpBodySetUserData(body, (void*) $(intptr_t ety));
    return body; } |]

setBodyType :: Ptr Body -> Body -> IO ()
setBodyType bodyPtr (fromIntegral . fromEnum -> bodyInt) =
  [C.exp| void { cpBodySetType($(cpBody* bodyPtr), $(int bodyInt)) } |]

getBodyType :: Ptr Body -> IO Body
getBodyType bodyPtr = toEnum . fromIntegral <$> [C.exp| int { cpBodyGetType($(cpBody* bodyPtr)) } |]

destroyBody :: SpacePtr -> Ptr Body -> IO ()
destroyBody spacePtr bodyPtr = withForeignPtr spacePtr $ \space -> [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpSpaceRemoveBody($(cpSpace* space), body);
  cpBodyFree(body); }|]

fromBodyPtr :: Ptr Body -> BodyRecord
fromBodyPtr ptr = BodyRecord ptr mempty mempty

instance Component Body where
  type Storage Body = Space Body

instance Has w Physics => Has w Body where
  getStore = (cast :: Space Physics -> Space Body) <$> getStore

instance Store (Space Body) where
  type Stores (Space Body) = Body
  type SafeRW (Space Body) = Maybe Body
  initStore = error "Initializing space from non-Physics store"

  explSet (Space bMap _ _ _ spcPtr) ety btype = do
    rd <- M.lookup ety <$> readIORef bMap
    bdyPtr <- case rd of
                Just (BodyRecord bdyPtr _ _) -> return bdyPtr
                Nothing -> do
                  bdyPtr <- newBody spcPtr ety
                  modifyIORef' bMap (M.insert ety $ fromBodyPtr bdyPtr)
                  return bdyPtr
    setBodyType bdyPtr btype

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of Nothing -> return Nothing
               Just (BodyRecord b _ _) -> Just <$> getBodyType b

  explDestroy sp@(Space bMap _ _ _ spc) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    modifyIORef' bMap (M.delete ety)
    forM_ rd $ \(BodyRecord bPtr shapes constraints) -> do
      forM_ (S.toList shapes) $ \s -> explDestroy (cast sp :: Space Shape) s
      forM_ (S.toList constraints) $ \s -> explDestroy (cast sp :: Space Constraint) s
      destroyBody spc bPtr

  explMembers (Space bMap _ _ _ _) = U.fromList . M.keys <$> readIORef bMap

  explExists (Space bMap _ _ _ _) ety = M.member ety <$> readIORef bMap

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    getBodyType b

  explSetMaybe = defaultSetMaybe

-- Position
getPosition :: Ptr Body -> IO (V2 Double)
getPosition bodyPtr = do
  x <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setPosition :: Ptr Body -> V2 Double -> IO ()
setPosition bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect pos = { $(double x), $(double y) };
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetPosition(body, pos);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  } |]

instance Component Position where
  type Storage Position = Space Position

instance Has w Physics => Has w Position where
  getStore = (cast :: Space Physics -> Space Position) <$> getStore

instance Store (Space Position) where
  type Stores (Space Position) = Position
  type SafeRW (Space Position) = Maybe Position
  initStore = error "Attempted to initialize a space from an Position component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . Position <$> getPosition b

  explSet (Space bMap _ _ _ _) ety (Position pos) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setPosition b pos

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    Position <$> getPosition b

-- Velocity
getVelocity :: Ptr Body -> IO (V2 Double)
getVelocity bodyPtr = do
  x <- [C.exp| double { cpBodyGetVelocity ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetVelocity ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setVelocity :: Ptr Body -> V2 Double -> IO ()
setVelocity bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vel = { $(double x), $(double y) };
  cpBodySetVelocity($(cpBody* bodyPtr), vel);
  } |]

instance Component Velocity where
  type Storage Velocity = Space Velocity

instance Has w Physics => Has w Velocity where
  getStore = (cast :: Space Physics -> Space Velocity) <$> getStore

instance Store (Space Velocity) where
  type Stores (Space Velocity) = Velocity
  type SafeRW (Space Velocity) = Maybe Velocity
  initStore = error "Attempted to initialize a space from an Velocity component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . Velocity <$> getVelocity b

  explSet (Space bMap _ _ _ _) ety (Velocity vel) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setVelocity b vel

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    Velocity <$> getVelocity b

-- Angle
getAngle :: Ptr Body -> IO Double
getAngle bodyPtr = do
  angle <- [C.exp| double { cpBodyGetAngle ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setAngle :: Ptr Body -> Double -> IO ()
setAngle bodyPtr (realToFrac -> angle) = [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetAngle(body, $(double angle));
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  } |]
  -- FIXME reindex

instance Component Angle where
  type Storage Angle = Space Angle

instance Has w Physics => Has w Angle where
  getStore = (cast :: Space Physics -> Space Angle) <$> getStore

instance Store (Space Angle) where
  type Stores (Space Angle) = Angle
  type SafeRW (Space Angle) = Maybe Angle
  initStore = error "Attempted to initialize a space from an Angle component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . Angle <$> getAngle b

  explSet (Space bMap _ _ _ _) ety (Angle angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setAngle b angle

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    Angle <$> getAngle b

-- AngularVelocity
getAngularVelocity :: Ptr Body -> IO Double
getAngularVelocity bodyPtr = do
  angle <- [C.exp| double { cpBodyGetAngularVelocity ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setAngularVelocity :: Ptr Body -> Double -> IO ()
setAngularVelocity bodyPtr (realToFrac -> angle) = [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetAngularVelocity(body, $(double angle));
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  } |]
  -- FIXME reindex

instance Component AngularVelocity where
  type Storage AngularVelocity = Space AngularVelocity

instance Has w Physics => Has w AngularVelocity where
  getStore = (cast :: Space Physics -> Space AngularVelocity) <$> getStore

instance Store (Space AngularVelocity) where
  type Stores (Space AngularVelocity) = AngularVelocity
  type SafeRW (Space AngularVelocity) = Maybe AngularVelocity
  initStore = error "Attempted to initialize a space from an AngularVelocity component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . AngularVelocity <$> getAngularVelocity b

  explSet (Space bMap _ _ _ _) ety (AngularVelocity angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setAngularVelocity b angle

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    AngularVelocity <$> getAngularVelocity b

-- Force
getForce :: Ptr Body -> IO (V2 Double)
getForce bodyPtr = do
  x <- [C.exp| double { cpBodyGetForce ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetForce ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setForce :: Ptr Body -> V2 Double -> IO ()
setForce bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect frc = { $(double x), $(double y) };
  cpBodySetForce($(cpBody* bodyPtr), frc);
  } |]

instance Component Force where
  type Storage Force = Space Force

instance Has w Physics => Has w Force where
  getStore = (cast :: Space Physics -> Space Force) <$> getStore

instance Store (Space Force) where
  type Stores (Space Force) = Force
  type SafeRW (Space Force) = Maybe Force
  initStore = error "Attempted to initialize a space from an Force component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . Force <$> getForce b

  explSet (Space bMap _ _ _ _) ety (Force frc) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setForce b frc

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    Force <$> getForce b

-- BodyMass
getBodyMass :: Ptr Body -> IO Double
getBodyMass bodyPtr = do
  angle <- [C.exp| double { cpBodyGetMass ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setBodyMass :: Ptr Body -> Double -> IO ()
setBodyMass bodyPtr (realToFrac -> angle) = [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetMass(body, $(double angle));
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  } |]
  -- FIXME reindex

instance Component BodyMass where
  type Storage BodyMass = Space BodyMass

instance Has w Physics => Has w BodyMass where
  getStore = (cast :: Space Physics -> Space BodyMass) <$> getStore

instance Store (Space BodyMass) where
  type Stores (Space BodyMass) = BodyMass
  type SafeRW (Space BodyMass) = Maybe BodyMass
  initStore = error "Attempted to initialize a space from an BodyMass component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . BodyMass <$> getBodyMass b

  explSet (Space bMap _ _ _ _) ety (BodyMass angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setBodyMass b angle

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    BodyMass <$> getBodyMass b

-- Moment
getMoment :: Ptr Body -> IO Double
getMoment bodyPtr = do
  angle <- [C.exp| double { cpBodyGetMoment ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setMoment :: Ptr Body -> Double -> IO ()
setMoment bodyPtr (realToFrac -> angle) = [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetMoment(body, $(double angle));
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  } |]
  -- FIXME reindex

instance Component Moment where
  type Storage Moment = Space Moment

instance Has w Physics => Has w Moment where
  getStore = (cast :: Space Physics -> Space Moment) <$> getStore

instance Store (Space Moment) where
  type Stores (Space Moment) = Moment
  type SafeRW (Space Moment) = Maybe Moment
  initStore = error "Attempted to initialize a space from an Moment component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . Moment <$> getMoment b

  explSet (Space bMap _ _ _ _) ety (Moment angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setMoment b angle

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    Moment <$> getMoment b

-- Torque
getTorque :: Ptr Body -> IO Double
getTorque bodyPtr = do
  angle <- [C.exp| double { cpBodyGetTorque ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setTorque :: Ptr Body -> Double -> IO ()
setTorque bodyPtr (realToFrac -> angle) = [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetTorque(body, $(double angle));
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  } |]
  -- FIXME reindex

instance Component Torque where
  type Storage Torque = Space Torque

instance Has w Physics => Has w Torque where
  getStore = (cast :: Space Physics -> Space Torque) <$> getStore

instance Store (Space Torque) where
  type Stores (Space Torque) = Torque
  type SafeRW (Space Torque) = Maybe Torque
  initStore = error "Attempted to initialize a space from an Torque component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . Torque <$> getTorque b

  explSet (Space bMap _ _ _ _) ety (Torque angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setTorque b angle

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    Torque <$> getTorque b

-- CenterOfGravity
getCenterOfGravity :: Ptr Body -> IO (V2 Double)
getCenterOfGravity bodyPtr = do
  x <- [C.exp| double { cpBodyGetCenterOfGravity ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetCenterOfGravity ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setCenterOfGravity :: Ptr Body -> V2 Double -> IO ()
setCenterOfGravity bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vel = { $(double x), $(double y) };
  cpBodySetCenterOfGravity($(cpBody* bodyPtr), vel);
  } |]

instance Component CenterOfGravity where
  type Storage CenterOfGravity = Space CenterOfGravity

instance Has w Physics => Has w CenterOfGravity where
  getStore = (cast :: Space Physics -> Space CenterOfGravity) <$> getStore

instance Store (Space CenterOfGravity) where
  type Stores (Space CenterOfGravity) = CenterOfGravity
  type SafeRW (Space CenterOfGravity) = Maybe CenterOfGravity
  initStore = error "Attempted to initialize a space from an CenterOfGravity component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just (BodyRecord b _ _)  -> Just . CenterOfGravity <$> getCenterOfGravity b

  explSet (Space bMap _ _ _ _) ety (CenterOfGravity vel) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setCenterOfGravity b vel

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    CenterOfGravity <$> getCenterOfGravity b

