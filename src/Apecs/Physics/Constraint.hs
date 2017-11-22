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

module Apecs.Physics.Constraint where

import           Apecs
import           Apecs.Stores        (defaultSetMaybe)
import           Apecs.Types
import           Control.Monad
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Body  ()
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

destroyConstraint :: Ptr Constraint -> IO ()
destroyConstraint constraintPtr = [C.block| void {
  cpConstraintDestroy ($(cpConstraint* constraintPtr));
  cpConstraintFree    ($(cpConstraint* constraintPtr)); }|]

instance Component Constraint where
  type Storage Constraint = Space Constraint

instance Has w Physics => Has w Constraint where
  getStore = (cast :: Space Physics -> Space Constraint) <$> getStore

instance Store (Space Constraint) where
  type Stores (Space Constraint) = Constraint
  type SafeRW (Space Constraint) = Maybe Constraint
  initStore = error "Initializing space from non-Physics store"

  explSet _ _ ConstraintRead = return ()
  explSet s ety (Constraint b ctype) = explSet s ety (ConstraintExtend (Entity ety) b ctype)
  explSet sp@(Space bMap _ cMap _ spcPtr) ety (ConstraintExtend (Entity a) (Entity b) ctype) = do
    explDestroy sp ety
    ea <- M.lookup a <$> readIORef bMap
    eb <- M.lookup b <$> readIORef bMap
    case (ea,eb) of
      (Just ba, Just bb) -> do
        cPtr <- newConstraint spcPtr ba bb ety ctype
        modifyIORef' cMap (M.insert ety cPtr)
      _ -> return ()

  explDestroy (Space _ _ cMap _ _) ety = do
    rd <- M.lookup ety <$> readIORef cMap
    modifyIORef' cMap (M.delete ety)
    forM_ rd destroyConstraint

  explMembers (Space _ _ cMap _ _) = U.fromList . M.keys <$> readIORef cMap

  explExists (Space _ _ cMap _ _) ety = M.member ety <$> readIORef cMap

  explSetMaybe = defaultSetMaybe
  explGet s ety = do
    e <- explExists s ety
    if e then Just <$> explGetUnsafe s ety else return Nothing
  explGetUnsafe _ _ = return (error "Constraint is a read-only component")


-- MaxForce
getMaxForce :: Ptr Constraint -> IO Double
getMaxForce c = do
  maxForce <- [C.exp| double { cpConstraintGetMaxForce ($(cpConstraint* c)) } |]
  return (realToFrac maxForce)

setMaxForce :: Ptr Constraint -> Double -> IO ()
setMaxForce c (realToFrac -> maxForce) = [C.exp| void { cpConstraintSetMaxForce($(cpConstraint* c), $(double maxForce)); } |]

instance Component MaxForce where
  type Storage MaxForce = Space MaxForce

instance Has w Physics => Has w MaxForce where
  getStore = (cast :: Space Physics -> Space MaxForce) <$> getStore

instance Store (Space MaxForce) where
  type Stores (Space MaxForce) = MaxForce
  type SafeRW (Space MaxForce) = Maybe MaxForce
  initStore = error "Initialize a space with a Physics component"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Constraint)
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ _ cMap _ _) ety = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return Nothing
      Just c  -> Just . MaxForce <$> getMaxForce c

  explSet (Space _ _ cMap _ _) ety (MaxForce vec) = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just c  -> setMaxForce c vec

  explGetUnsafe (Space _ _ cMap _ _) ety = do
    Just c <- M.lookup ety <$> readIORef cMap
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

instance Has w Physics => Has w MaxBias where
  getStore = (cast :: Space Physics -> Space MaxBias) <$> getStore

instance Store (Space MaxBias) where
  type Stores (Space MaxBias) = MaxBias
  type SafeRW (Space MaxBias) = Maybe MaxBias
  initStore = error "Initialize a space with a Physics component"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Constraint)
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ _ cMap _ _) ety = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return Nothing
      Just c  -> Just . MaxBias <$> getMaxBias c

  explSet (Space _ _ cMap _ _) ety (MaxBias vec) = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just c  -> setMaxBias c vec

  explGetUnsafe (Space _ _ cMap _ _) ety = do
    Just c <- M.lookup ety <$> readIORef cMap
    MaxBias <$> getMaxBias c

-- ErrorBias
getErrorBias :: Ptr Constraint -> IO Double
getErrorBias c = do
  errorBias <- [C.exp| double { cpConstraintGetErrorBias ($(cpConstraint* c)) } |]
  return (realToFrac errorBias)

setErrorBias :: Ptr Constraint -> Double -> IO ()
setErrorBias c (realToFrac -> errorBias) = [C.exp| void { cpConstraintSetErrorBias($(cpConstraint* c), $(double errorBias)); } |]

instance Component ErrorBias where
  type Storage ErrorBias = Space ErrorBias

instance Has w Physics => Has w ErrorBias where
  getStore = (cast :: Space Physics -> Space ErrorBias) <$> getStore

instance Store (Space ErrorBias) where
  type Stores (Space ErrorBias) = ErrorBias
  type SafeRW (Space ErrorBias) = Maybe ErrorBias
  initStore = error "Initialize a space with a Physics component"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Constraint)
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ _ cMap _ _) ety = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return Nothing
      Just c  -> Just . ErrorBias <$> getErrorBias c

  explSet (Space _ _ cMap _ _) ety (ErrorBias vec) = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just c  -> setErrorBias c vec

  explGetUnsafe (Space _ _ cMap _ _) ety = do
    Just c <- M.lookup ety <$> readIORef cMap
    ErrorBias <$> getErrorBias c

-- CollideBodies
getCollideBodies :: Ptr Constraint -> IO Bool
getCollideBodies c = do
  collide <- [C.exp| int { cpConstraintGetCollideBodies ($(cpConstraint* c)) } |]
  return . toEnum . fromIntegral $ collide

setCollideBodies :: Ptr Constraint -> Bool -> IO ()
setCollideBodies c (fromIntegral . fromEnum -> collide) = [C.exp| void { cpConstraintSetCollideBodies($(cpConstraint* c), $(int collide)); } |]

instance Component CollideBodies where
  type Storage CollideBodies = Space CollideBodies

instance Has w Physics => Has w CollideBodies where
  getStore = (cast :: Space Physics -> Space CollideBodies) <$> getStore

instance Store (Space CollideBodies) where
  type Stores (Space CollideBodies) = CollideBodies
  type SafeRW (Space CollideBodies) = Maybe CollideBodies
  initStore = error "Initialize a space with a Physics component"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Constraint)
  explExists s ety = explExists (cast s :: Space Constraint) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space _ _ cMap _ _) ety = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return Nothing
      Just c  -> Just . CollideBodies <$> getCollideBodies c

  explSet (Space _ _ cMap _ _) ety (CollideBodies vec) = do
    rd <- M.lookup ety <$> readIORef cMap
    case rd of
      Nothing -> return ()
      Just c  -> setCollideBodies c vec

  explGetUnsafe (Space _ _ cMap _ _) ety = do
    Just c <- M.lookup ety <$> readIORef cMap
    CollideBodies <$> getCollideBodies c
