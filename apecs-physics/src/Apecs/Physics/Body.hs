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
import           Control.Monad.IO.Class (liftIO, MonadIO)
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

instance Component Body where
  type Storage Body = Space Body

instance (MonadIO m, Has w m Physics) => Has w m Body where
  getStore = (cast :: Space Physics -> Space Body) <$> getStore

instance MonadIO m => ExplSet m (Space Body) where
  explSet (Space bMap _ _ _ spcPtr) ety btype = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    bdyPtr <- case rd of
                Just (BodyRecord bdyPtr _ _ _) -> return bdyPtr
                Nothing -> do
                  bdyPtr <- newBody spcPtr ety
                  bsMap <- newIORef mempty
                  bcMap <- newIORef mempty
                  modifyIORef' bMap (M.insert ety $ BodyRecord bdyPtr btype bsMap bcMap)
                  return bdyPtr
    setBodyType bdyPtr btype

instance MonadIO m => ExplDestroy m (Space Body) where
  explDestroy sp@(Space bMap _ _ _ spc) ety = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    modifyIORef' bMap (M.delete ety)
    forM_ rd $ \(BodyRecord bPtr _ shapes constraints) -> do
      readIORef shapes      >>= mapM_ (explDestroy (cast sp :: Space Shape))      . S.toList
      readIORef constraints >>= mapM_ (explDestroy (cast sp :: Space Constraint)) . S.toList
      destroyBody spc bPtr

instance MonadIO m => ExplMembers m (Space Body) where
  explMembers (Space bMap _ _ _ _) = liftIO $ U.fromList . M.keys <$> readIORef bMap

instance MonadIO m => ExplGet m (Space Body) where
  explExists (Space bMap _ _ _ _) ety = liftIO $ M.member ety <$> readIORef bMap
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord _ b _ _) <- M.lookup ety <$> readIORef bMap
    return b

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

instance (MonadIO m, Has w m Physics) => Has w m Position where
  getStore = (cast :: Space Physics -> Space Position) <$> getStore

instance MonadIO m => ExplMembers m (Space Position) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space Position) where
  explSet (Space bMap _ _ _ _) ety (Position pos) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _ _) -> setPosition b pos

instance MonadIO m => ExplGet m (Space Position) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m Velocity where
  getStore = (cast :: Space Physics -> Space Velocity) <$> getStore

instance MonadIO m => ExplMembers m (Space Velocity) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space Velocity) where
  explSet (Space bMap _ _ _ _) ety (Velocity vel) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _ _) -> setVelocity b vel

instance MonadIO m => ExplGet m (Space Velocity) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m Angle where
  getStore = (cast :: Space Physics -> Space Angle) <$> getStore

instance MonadIO m => ExplMembers m (Space Angle) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space Angle) where
  explSet (Space bMap _ _ _ _) ety (Angle angle) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _ _) -> setAngle b angle

instance MonadIO m => ExplGet m (Space Angle) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m AngularVelocity where
  getStore = (cast :: Space Physics -> Space AngularVelocity) <$> getStore

instance MonadIO m => ExplMembers m (Space AngularVelocity) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space AngularVelocity) where
  explSet (Space bMap _ _ _ _) ety (AngularVelocity angle) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _ _) -> setAngularVelocity b angle

instance MonadIO m => ExplGet m (Space AngularVelocity) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m Force where
  getStore = (cast :: Space Physics -> Space Force) <$> getStore

instance MonadIO m => ExplMembers m (Space Force) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space Force) where
  explSet (Space bMap _ _ _ _) ety (Force frc) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _ _) -> setForce b frc

instance MonadIO m => ExplGet m (Space Force) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m BodyMass where
  getStore = (cast :: Space Physics -> Space BodyMass) <$> getStore

instance MonadIO m => ExplMembers m (Space BodyMass) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space BodyMass) where
  explSet (Space bMap _ _ _ _) ety (BodyMass angle) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _ _) -> setBodyMass b angle

instance MonadIO m => ExplGet m (Space BodyMass) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m Moment where
  getStore = (cast :: Space Physics -> Space Moment) <$> getStore

instance MonadIO m => ExplMembers m (Space Moment) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space Moment) where
  explSet (Space bMap _ _ _ _) ety (Moment angle) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _ _) -> setMoment b angle

instance MonadIO m => ExplGet m (Space Moment) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m Torque where
  getStore = (cast :: Space Physics -> Space Torque) <$> getStore

instance MonadIO m => ExplMembers m (Space Torque) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space Torque) where
  explSet (Space bMap _ _ _ _) ety (Torque angle) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \(BodyRecord b _ _ _) -> setTorque b angle

instance MonadIO m => ExplGet m (Space Torque) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
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

instance (MonadIO m, Has w m Physics) => Has w m CenterOfGravity where
  getStore = (cast :: Space Physics -> Space CenterOfGravity) <$> getStore

instance MonadIO m => ExplMembers m (Space CenterOfGravity) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplSet m (Space CenterOfGravity) where
  explSet (Space bMap _ _ _ _) ety (CenterOfGravity vel) = liftIO $ do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \(BodyRecord b _ _ _) -> setCenterOfGravity b vel

instance MonadIO m => ExplGet m (Space CenterOfGravity) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord b _ _ _) <- M.lookup ety <$> readIORef bMap
    CenterOfGravity <$> getCenterOfGravity b

-- ShapeList
instance Component ShapeList where
  type Storage ShapeList = Space ShapeList

instance (MonadIO m, Has w m Physics) => Has w m ShapeList where
  getStore = (cast :: Space Physics -> Space ShapeList) <$> getStore

instance MonadIO m => ExplMembers m (Space ShapeList) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplGet m (Space ShapeList) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord _ _ sPtr _) <- M.lookup ety <$> readIORef bMap
    ShapeList . fmap Entity . S.toList <$> readIORef sPtr

-- ConstraintList
instance Component ConstraintList where
  type Storage ConstraintList = Space ConstraintList

instance (MonadIO m, Has w m Physics) => Has w m ConstraintList where
  getStore = (cast :: Space Physics -> Space ConstraintList) <$> getStore

instance MonadIO m => ExplMembers m (Space ConstraintList) where
  explMembers s = explMembers (cast s :: Space Body)

instance MonadIO m => ExplGet m (Space ConstraintList) where
  explExists s ety = explExists (cast s :: Space Body) ety
  explGet (Space bMap _ _ _ _) ety = liftIO $ do
    Just (BodyRecord _ _ _ cPtr) <- M.lookup ety <$> readIORef bMap
    ConstraintList . fmap Entity . S.toList <$> readIORef cPtr
