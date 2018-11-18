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
import           Apecs.Core
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

instance Has w IO Physics => Has w IO Body where
  getStore = (cast :: Space Physics -> Space Body) <$> getStore

instance ExplSet IO (Space Body) where
  explSet (Space bMap _ _ _ spcPtr) ety btype = do
    rd <- M.lookup ety <$> readIORef bMap
    bdyPtr <- case rd of
                Just (BodyRecord bdyPtr _ _) -> return bdyPtr
                Nothing -> do
                  bdyPtr <- newBody spcPtr ety
                  modifyIORef' bMap (M.insert ety $ fromBodyPtr bdyPtr)
                  return bdyPtr
    setBodyType bdyPtr btype

instance ExplDestroy IO (Space Body) where
  explDestroy sp@(Space bMap _ _ _ spc) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    modifyIORef' bMap (M.delete ety)
    forM_ rd $ \(BodyRecord bPtr shapes constraints) -> do
      forM_ (S.toList shapes) $ \s -> explDestroy (cast sp :: Space Shape) s
      forM_ (S.toList constraints) $ \s -> explDestroy (cast sp :: Space Constraint) s
      destroyBody spc bPtr

instance ExplMembers IO (Space Body) where
  explMembers (Space bMap _ _ _ _) = U.fromList . M.keys <$> readIORef bMap

instance ExplGet IO (Space Body) where
  explExists (Space bMap _ _ _ _) ety = M.member ety <$> readIORef bMap

  explGet (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    getBodyType b

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

instance Has w IO Physics => Has w IO Position where
  getStore = (cast :: Space Physics -> Space Position) <$> getStore

instance ExplMembers IO (Space Position) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space Position) where
  explSet (Space bMap _ _ _ _) ety (Position pos) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setPosition b pos

instance ExplGet IO (Space Position) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO Velocity where
  getStore = (cast :: Space Physics -> Space Velocity) <$> getStore

instance ExplMembers IO (Space Velocity) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space Velocity) where
  explSet (Space bMap _ _ _ _) ety (Velocity vel) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setVelocity b vel

instance ExplGet IO (Space Velocity) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO Angle where
  getStore = (cast :: Space Physics -> Space Angle) <$> getStore

instance ExplMembers IO (Space Angle) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space Angle) where
  explSet (Space bMap _ _ _ _) ety (Angle angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setAngle b angle

instance ExplGet IO (Space Angle) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO AngularVelocity where
  getStore = (cast :: Space Physics -> Space AngularVelocity) <$> getStore

instance ExplMembers IO (Space AngularVelocity) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space AngularVelocity) where
  explSet (Space bMap _ _ _ _) ety (AngularVelocity angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setAngularVelocity b angle

instance ExplGet IO (Space AngularVelocity) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO Force where
  getStore = (cast :: Space Physics -> Space Force) <$> getStore

instance ExplMembers IO (Space Force) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space Force) where
  explSet (Space bMap _ _ _ _) ety (Force frc) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setForce b frc

instance ExplGet IO (Space Force) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO BodyMass where
  getStore = (cast :: Space Physics -> Space BodyMass) <$> getStore

instance ExplMembers IO (Space BodyMass) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space BodyMass) where
  explSet (Space bMap _ _ _ _) ety (BodyMass angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setBodyMass b angle

instance ExplGet IO (Space BodyMass) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO Moment where
  getStore = (cast :: Space Physics -> Space Moment) <$> getStore

instance ExplMembers IO (Space Moment) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space Moment) where
  explSet (Space bMap _ _ _ _) ety (Moment angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setMoment b angle

instance ExplGet IO (Space Moment) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO Torque where
  getStore = (cast :: Space Physics -> Space Torque) <$> getStore

instance ExplMembers IO (Space Torque) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space Torque) where
  explSet (Space bMap _ _ _ _) ety (Torque angle) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _) -> setTorque b angle

instance ExplGet IO (Space Torque) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
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

instance Has w IO Physics => Has w IO CenterOfGravity where
  getStore = (cast :: Space Physics -> Space CenterOfGravity) <$> getStore

instance ExplMembers IO (Space CenterOfGravity) where
  explMembers s = explMembers (cast s :: Space Body)

instance ExplSet IO (Space CenterOfGravity) where
  explSet (Space bMap _ _ _ _) ety (CenterOfGravity vel) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _) -> setCenterOfGravity b vel

instance ExplGet IO (Space CenterOfGravity) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef bMap
    CenterOfGravity <$> getCenterOfGravity b

