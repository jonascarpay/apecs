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
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Body
import           Apecs.Physics.Space
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
    cpSpaceAddConstraint($(cpSpace* space), constraint);
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    cpConstraintSetCollideBodies(constraint, 0);
    return constraint; } |]

newConstraint spacePtr bodyA bodyB (fromIntegral -> ety)
              (PivotJoint (fmap realToFrac -> V2 x y)) =
  withForeignPtr spacePtr $ \space -> [C.block| cpConstraint* {
    cpVect anchor = cpv( $(double x), $(double y) );
    cpConstraint* constraint = cpPivotJointNew($(cpBody* bodyA), $(cpBody* bodyB), anchor);
    cpSpaceAddConstraint($(cpSpace* space), constraint);
    cpConstraintSetUserData(constraint, (void*) $(intptr_t ety));
    cpConstraintSetCollideBodies(constraint, 0);
    return constraint; } |]

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

  explSet sp@(Space bMap cMap _ spcPtr) ety (Constraint (Entity a) (Entity b) ctype) = do
    explDestroy sp ety
    rd <- M.lookup ety <$> readIORef cMap
    ea <- M.lookup a <$> readIORef bMap
    eb <- M.lookup b <$> readIORef bMap
    case (ea,eb) of
      (Just (BodyRecord ba _ _), Just (BodyRecord bb _ _)) -> do
        cPtr <- newConstraint spcPtr ba bb ety ctype
        modifyIORef' cMap (M.insert ety cPtr)
      _ -> return ()

  explDestroy (Space _ cMap _ _) ety = do
    rd <- M.lookup ety <$> readIORef cMap
    modifyIORef' cMap (M.delete ety)
    case rd of Just c -> destroyConstraint c
               _      -> return ()

  explMembers (Space _ cMap _ _) = U.fromList . M.keys <$> readIORef cMap

  explExists (Space _ cMap _ _) ety = M.member ety <$> readIORef cMap

  explSetMaybe = defaultSetMaybe
