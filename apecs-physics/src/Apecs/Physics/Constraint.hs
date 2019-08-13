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

module Apecs.Physics.Constraint
  (
  ) where

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
import           Apecs.Physics.Types

C.context phycsCtx
C.include "<chipmunk.h>"

-- Constraint
newConstraint :: SpacePtr -> Ptr Body -> Ptr Body -> Int -> ConstraintType -> IO (Ptr Constraint)
newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (PinJoint (fmap realToFrac -> V2 ax ay) (fmap realToFrac -> V2 bx by)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect anchorA = cpv( $(double ax), $(double ay) );
    cpVect anchorB = cpv( $(double bx), $(double by) );
    cpConstraint* constraint = cpPinJointNew($(cpBody* bodyA), $(cpBody* bodyB),anchorA,anchorB);
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (SlideJoint (fmap realToFrac -> V2 ax ay) (fmap realToFrac -> V2 bx by) (realToFrac -> min) (realToFrac -> max)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect anchorA = cpv( $(double ax), $(double ay) );
    cpVect anchorB = cpv( $(double bx), $(double by) );
    cpConstraint* constraint = cpSlideJointNew($(cpBody* bodyA), $(cpBody* bodyB),anchorA,anchorB,$(double min),$(double max));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (PivotJoint (fmap realToFrac -> V2 x y)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect anchor = cpv( $(double x), $(double y) );
    cpConstraint* constraint = cpPivotJointNew($(cpBody* bodyA), $(cpBody* bodyB), anchor);
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (PivotJoint2 (fmap realToFrac -> V2 ax ay) (fmap realToFrac -> V2 bx by)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect va = cpv( $(double ax), $(double ay) );
    cpVect vb = cpv( $(double bx), $(double by) );
    cpConstraint* constraint = cpPivotJointNew2($(cpBody* bodyA), $(cpBody* bodyB), va, vb);
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (GrooveJoint (fmap realToFrac -> V2 ax ay) (fmap realToFrac -> V2 bx by) (fmap realToFrac -> V2 ancx ancy)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect va = cpv( $(double ax), $(double ay) );
    cpVect vb = cpv( $(double bx), $(double by) );
    cpVect anchor = cpv( $(double ancx), $(double ancy) );
    cpConstraint* constraint = cpGrooveJointNew($(cpBody* bodyA), $(cpBody* bodyB), va, vb, anchor);
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (DampedSpring (fmap realToFrac -> V2 ax ay) (fmap realToFrac -> V2 bx by) (realToFrac -> rl) (realToFrac -> stf) (realToFrac -> damping)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect va = cpv( $(double ax), $(double ay) );
    cpVect vb = cpv( $(double bx), $(double by) );
    cpConstraint* constraint = cpDampedSpringNew($(cpBody* bodyA), $(cpBody* bodyB), va, vb, $(double rl), $(double stf), $(double damping));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (DampedRotarySpring (realToFrac -> ra) (realToFrac -> stf) (realToFrac -> damping)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpConstraint* constraint = cpDampedRotarySpringNew($(cpBody* bodyA), $(cpBody* bodyB), $(double ra), $(double stf), $(double damping));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (RotaryLimitJoint  (realToFrac -> min) (realToFrac -> max)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpConstraint* constraint = cpRotaryLimitJointNew($(cpBody* bodyA), $(cpBody* bodyB), $(double min), $(double max));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (RatchetJoint (realToFrac -> phase) (realToFrac -> ratchet)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpConstraint* constraint = cpRatchetJointNew($(cpBody* bodyA), $(cpBody* bodyB), $(double phase), $(double ratchet));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (GearJoint (realToFrac -> phase) (realToFrac -> ratio)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpConstraint* constraint = cpGearJointNew($(cpBody* bodyA), $(cpBody* bodyB), $(double phase), $(double ratio));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (SimpleMotor (realToFrac -> rate)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpConstraint* constraint = cpSimpleMotorNew($(cpBody* bodyA), $(cpBody* bodyB), $(double rate));
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    return cpSpaceAddConstraint($(cpSpace* space), constraint);
    } |]

destroyConstraint :: SpacePtr -> Ptr Constraint -> IO ()
destroyConstraint spacePtr constraintPtr = withForeignPtr spacePtr $ \space -> [C.block| void {
  cpConstraint *constraint = $(cpConstraint* constraintPtr);
  cpSpaceRemoveConstraint($(cpSpace* space), constraint);
  cpConstraintFree(constraint); }|]

instance Component Constraint where
  type Storage Constraint = Space Constraint

instance (MonadIO m, Has w m Physics) => Has w m Constraint where
  getStore = (cast :: Space Physics -> Space Constraint) <$> getStore

instance MonadIO m => ExplSet m (Space Constraint) where
  explSet sp@(Space bMap _ cMap _ spcPtr) cEty cons@(Constraint (Entity bEtyA) (Entity bEtyB) ctype) = liftIO $ do
    explDestroy sp cEty
    mBrA <- M.lookup bEtyA <$> readIORef bMap
    mBrB <- M.lookup bEtyB <$> readIORef bMap
    case (mBrA,mBrB) of
      (Just brA, Just brB) -> do
        cPtr <- newConstraint spcPtr (brPtr brA) (brPtr brB) cEty ctype

        modifyIORef' cMap (M.insert cEty (Record cPtr cons))
        modifyIORef' (brConstraints brA) (S.insert cEty)
        modifyIORef' (brConstraints brB) (S.insert cEty)

      _ -> return ()

instance MonadIO m => ExplDestroy m (Space Constraint) where
  explDestroy (Space bMap _ cMap _ spc) cEty = liftIO $ do
    rd <- M.lookup cEty <$> readIORef cMap
    forM_ rd $ \(Record cPtr (Constraint (Entity bEtyA) (Entity bEtyB) _)) -> do
      bMap' <- readIORef bMap
      let rmConstraint ref = modifyIORef' (brConstraints ref) (S.delete cEty)
      mapM_ rmConstraint (M.lookup bEtyA bMap')
      mapM_ rmConstraint (M.lookup bEtyB bMap')
      modifyIORef' cMap $ M.delete cEty
      destroyConstraint spc cPtr

instance MonadIO m => ExplMembers m (Space Constraint) where
  explMembers (Space _ _ cMap _ _) = liftIO $ U.fromList . M.keys <$> readIORef cMap

instance MonadIO m => ExplGet m (Space Constraint) where
  explExists (Space _ _ cMap _ _) ety = liftIO $ M.member ety <$> readIORef cMap
  explGet    (Space _ _ cMap _ _) ety = liftIO $ do
    Just (Record _ cons)  <- M.lookup ety <$> readIORef cMap
    return cons

-- MaxForce
getMaxForce :: Ptr Constraint -> IO Double
getMaxForce c = realToFrac <$> [C.exp| double { cpConstraintGetMaxForce ($(cpConstraint* c)) } |]

setMaxForce :: Ptr Constraint -> Double -> IO ()
setMaxForce c (realToFrac -> maxForce) = [C.exp| void { cpConstraintSetMaxForce($(cpConstraint* c), $(double maxForce)); } |]

instance Component MaxForce where
  type Storage MaxForce = Space MaxForce

instance (MonadIO m, Has w m Physics) => Has w m MaxForce where
  getStore = (cast :: Space Physics -> Space MaxForce) <$> getStore

instance MonadIO m => ExplMembers m (Space MaxForce) where
  explMembers s = explMembers (cast s :: Space Constraint)

instance MonadIO m => ExplSet m (Space MaxForce) where
  explSet (Space _ _ cMap _ _) ety (MaxForce vec) = liftIO $ do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just (Record c _) -> setMaxForce c vec

instance MonadIO m => ExplGet m (Space MaxForce) where
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explGet (Space _ _ cMap _ _) ety = liftIO $ do
    Just (Record c _) <- M.lookup ety <$> readIORef cMap
    MaxForce <$> getMaxForce c

-- MaxBias
getMaxBias :: Ptr Constraint -> IO Double
getMaxBias c = do
  maxBias <- [C.exp| double { cpConstraintGetMaxBias ($(cpConstraint* c)) } |]
  return (realToFrac maxBias)

setMaxBias :: Ptr Constraint -> Double -> IO ()
setMaxBias c (realToFrac -> maxBias) = [C.exp| void { cpConstraintSetMaxBias($(cpConstraint* c), $(double maxBias)); } |]

instance Component MaxBias where
  type Storage MaxBias = Space MaxBias

instance (MonadIO m, Has w m Physics) => Has w m MaxBias where
  getStore = (cast :: Space Physics -> Space MaxBias) <$> getStore

instance MonadIO m => ExplMembers m (Space MaxBias) where
  explMembers s = explMembers (cast s :: Space Constraint)

instance MonadIO m => ExplSet m (Space MaxBias) where
  explSet (Space _ _ cMap _ _) ety (MaxBias vec) = liftIO $ do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just (Record c _) -> setMaxBias c vec

instance MonadIO m => ExplGet m (Space MaxBias) where
  explGet (Space _ _ cMap _ _) ety = liftIO $ do
    Just (Record c _) <- M.lookup ety <$> readIORef cMap
    MaxBias <$> getMaxBias c
  explExists s ety = explExists (cast s :: Space Constraint) ety

-- ErrorBias
getErrorBias :: Ptr Constraint -> IO Double
getErrorBias c = liftIO $ do
  errorBias <- [C.exp| double { cpConstraintGetErrorBias ($(cpConstraint* c)) } |]
  return (realToFrac errorBias)

setErrorBias :: Ptr Constraint -> Double -> IO ()
setErrorBias c (realToFrac -> errorBias) = [C.exp| void { cpConstraintSetErrorBias($(cpConstraint* c), $(double errorBias)); } |]

instance Component ErrorBias where
  type Storage ErrorBias = Space ErrorBias

instance (MonadIO m, Has w m Physics) => Has w m ErrorBias where
  getStore = (cast :: Space Physics -> Space ErrorBias) <$> getStore

instance MonadIO m => ExplMembers m (Space ErrorBias) where
  explMembers s = explMembers (cast s :: Space Constraint)

instance MonadIO m => ExplSet m (Space ErrorBias) where
  explSet (Space _ _ cMap _ _) ety (ErrorBias vec) = liftIO $ do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just (Record c _) -> setErrorBias c vec

instance MonadIO m => ExplGet m (Space ErrorBias) where
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explGet (Space _ _ cMap _ _) ety = liftIO $ do
    Just (Record c _) <- M.lookup ety <$> readIORef cMap
    ErrorBias <$> getErrorBias c

-- Impulse
getImpulse :: Ptr Constraint -> IO Double
getImpulse c = realToFrac <$> [C.exp| double { cpConstraintGetImpulse ($(cpConstraint* c)) } |]

instance Component Impulse where
  type Storage Impulse = Space Impulse

instance (MonadIO m, Has w m Physics) => Has w m Impulse where
  getStore = (cast :: Space Physics -> Space Impulse) <$> getStore

instance MonadIO m => ExplMembers m (Space Impulse) where
  explMembers s = explMembers (cast s :: Space Constraint)

instance MonadIO m => ExplGet m (Space Impulse) where
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explGet (Space _ _ cMap _ _) ety = liftIO $ do
    Just (Record c _) <- M.lookup ety <$> readIORef cMap
    Impulse <$> getImpulse c

-- CollideBodies
getCollideBodies :: Ptr Constraint -> IO Bool
getCollideBodies c = do
  collide <- [C.exp| int { cpConstraintGetCollideBodies ($(cpConstraint* c)) } |]
  return . toEnum . fromIntegral $ collide

setCollideBodies :: Ptr Constraint -> Bool -> IO ()
setCollideBodies c (fromIntegral . fromEnum -> collide) = [C.exp| void { cpConstraintSetCollideBodies($(cpConstraint* c), $(int collide)); } |]

instance Component CollideBodies where
  type Storage CollideBodies = Space CollideBodies

instance (MonadIO m, Has w m Physics) => Has w m CollideBodies where
  getStore = (cast :: Space Physics -> Space CollideBodies) <$> getStore

instance MonadIO m => ExplMembers m (Space CollideBodies) where
  explMembers s = explMembers (cast s :: Space Constraint)

instance MonadIO m => ExplSet m (Space CollideBodies) where
  explSet (Space _ _ cMap _ _) ety (CollideBodies vec) = liftIO $ do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just (Record c _) -> setCollideBodies c vec

instance MonadIO m => ExplGet m (Space CollideBodies) where
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explGet (Space _ _ cMap _ _) ety = liftIO $ do
    Just (Record c _) <- M.lookup ety <$> readIORef cMap
    CollideBodies <$> getCollideBodies c
