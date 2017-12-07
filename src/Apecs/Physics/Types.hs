{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Apecs.Physics.Types where

import           Apecs
import           Apecs.Types
import           Data.Bits
import           Data.Char                 (intToDigit)
import qualified Data.IntMap               as M
import qualified Data.IntSet               as S
import           Data.IORef
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import qualified Foreign.C.Types           as C
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types          as C
import qualified Language.Haskell.TH       as TH
import           Linear.V2
import           Numeric                   (showIntAtBase)

phycsCtx :: Context
phycsCtx = baseCtx <> funCtx <> ctx
  where ctx = mempty { ctxTypesTable = phycsTypesTable }

phycsTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
phycsTypesTable = Map.fromList
  [ (C.TypeName "cpArbiter",          [t| Collision        |])
  , (C.TypeName "cpBody",             [t| Body             |])
  , (C.TypeName "cpCollisionHandler", [t| CollisionHandler |])
  , (C.TypeName "cpConstraint",       [t| Constraint       |])
  , (C.TypeName "cpDataPointer",      [t| C.CUInt          |])
  , (C.TypeName "cpShape",            [t| Shape            |])
  , (C.TypeName "cpPointQueryInfo",   [t| PointQueryResult |])
  , (C.TypeName "cpVect",             [t| V2 C.CDouble     |])
  , (C.TypeName "cpSpace",            [t| FrnSpace         |])
  ]

-- | Uninhabited data type for constructing a world with a chipmunk space.
data Physics

-- | Vector type used by the library
type Vec = V2 Double
-- | Type synonym indicating that a vector is expected to be in body-space coordinates
type BVec = Vec
-- | Type synonym indicating that a vector is expected to be in world-space coordinates
type WVec = Vec

-- | Added to a component to add it to the physics space.
--   Deleting it will also delete all associated shapes and constraints.
--   A body has a number of subcomponents: @Position@, @Velocity@, @Force@, @Torque@, @BodyMass@, @Moment@, @Angle@, @AngularVelocity@, and @CenterOfGravity@.
--   These components cannot be added or removed from an entity, but rather are present as long as the entity has a @Body@.
data Body = DynamicBody | KinematicBody | StaticBody deriving (Eq, Ord, Enum)

newtype Position        = Position WVec
newtype Velocity        = Velocity WVec
newtype Force           = Force Vec
newtype Torque          = Torque Double
newtype BodyMass        = BodyMass Double deriving (Eq, Show)
newtype Moment          = Moment Double deriving (Eq, Show)
newtype Angle           = Angle Double deriving (Eq, Show)
newtype AngularVelocity = AngularVelocity Double
newtype CenterOfGravity = CenterOfGravity BVec

-- | Shape component.
--   Adding a shape to an entity that has no @Body@ is a noop.
data Shape = Shape Convex
           | ShapeExtend (Entity Body) Convex
           | ShapeRead -- ^ Shapes are write-only, this is returned when you attempt to read

-- | A convex polygon.
--   Consists of a list of vertices, and a radius.
data Convex = Convex [BVec] Double deriving (Eq, Show)

newtype Sensor          = Sensor          Bool          deriving (Eq, Show)
newtype Elasticity      = Elasticity      Double        deriving (Eq, Show)
newtype Mass            = Mass            Double        deriving (Eq, Show)
newtype Density         = Density         Double        deriving (Eq, Show)
newtype Friction        = Friction        Double        deriving (Eq, Show)
newtype SurfaceVelocity = SurfaceVelocity Vec           deriving (Eq, Show)
newtype CollisionType   = CollisionType   C.CUIntPtr    deriving (Eq, Show)
newtype ShapeBody       = ShapeBody       (Entity Body) deriving (Eq, Show)

type CollisionGroup = CUInt

data CollisionFilter = CollisionFilter
  { filterGroup      :: CollisionGroup
  , filterCategories :: Bitmask
  , filterMask       :: Bitmask
  } deriving (Eq, Show)

newtype Bitmask = Bitmask CUInt deriving (Eq, Bits)
instance Show Bitmask where
  show (Bitmask mask) = "Bitmask " ++ showIntAtBase 2 intToDigit mask ""

data FrnSpace
data FrnVec

data Space c = Space
  { spBodies      :: IOMap BodyRecord
  , spShapes      :: PtrMap Shape
  , spConstraints :: PtrMap Constraint
  , spHandlers    :: PtrMap CollisionHandler
  , spacePtr      :: SpacePtr
  }

data BodyRecord = BodyRecord
  { brPtr         :: Ptr Body
  , brShapes      :: S.IntSet
  , brConstraints :: S.IntSet
  }

type IOMap a = IORef (M.IntMap a)
type PtrMap a = IOMap (Ptr a)
type SpacePtr = ForeignPtr FrnSpace

-- Space subcomponents
newtype Iterations = Iterations Int
newtype Gravity = Gravity Vec deriving (Eq, Show)
newtype Damping = Damping Double
newtype IdleSpeedThreshold = IdleSpeedThreshold Double
newtype SleepIdleTime = SleepIdleTime Double
newtype CollisionSlop = CollisionSlop Double
newtype CollisionBias = CollisionBias Double

instance Cast Space where
  cast (Space b s c h w) = Space b s c h w

-- Constraint subcomponents
newtype MaxForce      = MaxForce      Double
newtype MaxBias       = MaxBias       Double
newtype ErrorBias     = ErrorBias     Double
newtype CollideBodies = CollideBodies Bool

data Constraint = Constraint (Entity Body) ConstraintType
                | ConstraintExtend (Entity Body) (Entity Body) ConstraintType
                | ConstraintRead

data ConstraintType
  = PinJoint BVec BVec -- ^ Maintains a fixed distance between two anchor points
  | SlideJoint BVec BVec Double Double -- offsetA offsetB min max
  | PivotJoint WVec -- ^ Creates a pivot point at the given world coordinate
  | PivotJoint2 BVec BVec -- ^ Creates a pivot point at the given body coordinates
  | GrooveJoint BVec BVec BVec
  | DampedSpring BVec BVec Double Double Double -- offA offB restlength stiffness damping
  | DampedRotarySpring Double Double Double -- restAngle stiffness damping
  | RotaryLimitJoint Double Double -- min max
  | RatchetJoint Double Double -- phase ratchet
  | GearJoint Double Double -- phase ratio
  | SimpleMotor Double -- rate

-- TODO
-- getConstraintImpulse
-- getPinJointDistance
-- getSlideJointDistance?


newtype BeginCB     = BeginCB     BeginFunc
newtype SeparateCB  = SeparateCB  SeparateFunc
newtype PreSolveCB  = PreSolveCB  PreSolveFunc
newtype PostSolveCB = PostSolveCB PostSolveFunc

-- Collision, Space, Handler data pointer
type BeginFunc     = Ptr Collision -> Ptr FrnSpace -> C.CUInt -> IO C.CUChar
type SeparateFunc  = Ptr Collision -> Ptr FrnSpace -> C.CUInt -> IO ()
type PreSolveFunc  = Ptr Collision -> Ptr FrnSpace -> C.CUInt -> IO C.CUChar
type PostSolveFunc = Ptr Collision -> Ptr FrnSpace -> C.CUInt -> IO ()

data CollisionHandler = CollisionHandler
  { source      :: CollisionSource
  , beginCB     :: Maybe BeginCB
  , separateCB  :: Maybe SeparateCB
  , preSolveCB  :: Maybe PreSolveCB
  , postSolveCB :: Maybe PostSolveCB
  }

data CollisionSource
  = Wildcard CollisionGroup
  | Between CollisionGroup CollisionGroup

-- Corresponds to an 'arbiter' in Chipmunk
data Collision = Collision
  { collisionNormal :: Vec
  , collisionA      :: Entity Body
  , collisionB      :: Entity Body
  } deriving (Eq, Show)

data CollisionProperties = CollisionProperties
  { collisionElasticity      :: Double
  , collisionFriction        :: Double
  , collisionSurfaceVelocity :: Vec
  } deriving (Eq, Show)

data SegmentQueryResult = SegmentQueryResult
  { sqShape        :: Entity Shape
  , sqImpactPoint  :: Vec
  , sqImpactNormal :: Vec
  , sqImpactAlpha  :: Double
  } deriving (Eq, Show)

data PointQueryResult = PointQueryResult
  { pqShape    :: Entity Shape
  , pqPoint    :: WVec
  , pqDistance :: Double
  , pqGradient :: Double
  } deriving (Eq, Show)

