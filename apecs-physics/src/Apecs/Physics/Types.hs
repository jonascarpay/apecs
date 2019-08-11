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
import           Apecs.Core
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

-- | Uninhabited, should be added to the world as a component to add a physics space.
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

-- | A subcomponent of @Body@ representing where it is in world coordinates.
newtype Position        = Position WVec
-- | A subcomponent of @Body@ representing where it is going in world coordinates
newtype Velocity        = Velocity WVec
-- | A component used to apply a force to a @Body@.
-- The force is applied to the body's center of gravity.
-- This component is reset to @ Vec 0 0 @ after every stimulation step, 
-- so it is mainly used to apply a force as opposed to being read.
newtype Force           = Force Vec
-- | A component used to apply a torque to a @Body@.
-- The torque is applied to the entire body at once.
-- This component is reset to @ 0 @ after every simulation step, so it
-- is mainly used to apply a torque as opposed to being read.
newtype Torque          = Torque Double
-- | A component representing the mass of the @Body@ overall.
newtype BodyMass        = BodyMass Double deriving (Eq, Show)
-- | The moment of inertia of the @Body@.
-- This is basically the body's tendency to resist angular acceleration.
newtype Moment          = Moment Double deriving (Eq, Show)
newtype Angle           = Angle Double deriving (Eq, Show)
newtype AngularVelocity = AngularVelocity Double
-- | Where the @Body@'s center of gravity is, in body-local coordinates.
-- Can be read and written to.
newtype CenterOfGravity = CenterOfGravity BVec

-- | The @Shape@s belonging to a body. Read-only.
newtype ShapeList = ShapeList [Entity]
-- | The @Constraint@s belonging to a body. Read-only.
newtype ConstraintList = ConstraintList [Entity]

-- | Shape component.
--   Adding a shape to an entity that has no @Body@ is a noop.
data Shape = Shape Entity Convex

-- | A convex polygon.
--   Consists of a list of vertices, and a radius.
data Convex = Convex [BVec] Double deriving (Eq, Show)

-- | If a body is a 'Sensor', it exists only to trigger collision responses.
-- It won't phyiscally interact with other bodies in any way, but it __will__
-- cause collision handlers to run. 
newtype Sensor          = Sensor          Bool       deriving (Eq, Show)
-- | The elasticity of a shape. Higher elasticities will create more
-- elastic collisions, IE, will be bouncier.
--
-- See <https://en.wikipedia.org/wiki/Elasticity_(physics)> for more information.
newtype Elasticity      = Elasticity      Double     deriving (Eq, Show)
-- | The mass of a shape is technically a measure of how much resistance it has to
-- being accelerated, but it's generally easier to understand it as being how "heavy" something is.
--
-- The physics engine lets you set this, and it will calculate the 'Density' and other components
-- for you. 
--
-- See <https://en.wikipedia.org/wiki/Mass> for more information.
newtype Mass            = Mass            Double     deriving (Eq, Show)
-- | The density of a shape is a measure of how much mass an object has in a given volume.
-- 
-- The physics engine lets you set this, and it will calculate the 'Mass' and other components for you.
-- 
-- See <https://en.wikipedia.org/wiki/Density> for more information.
newtype Density         = Density         Double     deriving (Eq, Show)
-- | The friction of an object is a measure of how much it resists movement.
-- Shapes with high friction will naturally slow down more quickly over time than objects
-- with low friction.
--
-- See <https://en.wikipedia.org/wiki/Friction> for more information.
newtype Friction        = Friction        Double     deriving (Eq, Show)
newtype SurfaceVelocity = SurfaceVelocity Vec        deriving (Eq, Show)

type CollisionGroup = CUInt

-- | Collision Filters determine what shapes this shape collides with.
--   Shapes in the same 'filterGroup' will never collide with one another.
--   This is used to ignore collisions between parts on a complex object.
--
--   'filterCategories' is a bitmask that determines what categories a shape belongs to.
--   'filterMask' is a bitmask that determines what categories it collides with.
--   See <https://chipmunk-physics.net/release/ChipmunkLatest-Docs/#cpShape-Filtering> for more information.
data CollisionFilter = CollisionFilter
  { filterGroup      :: CollisionGroup
  , filterCategories :: Bitmask
  , filterMask       :: Bitmask
  } deriving (Eq, Show)

-- | A bitmask used for collision handling
newtype Bitmask = Bitmask CUInt deriving (Eq, Bits)
instance Show Bitmask where
  show (Bitmask mask) = "Bitmask 0b" ++ showIntAtBase 2 intToDigit mask ""

data FrnSpace
data FrnVec

data Space c = Space
  { spBodies      :: IOMap BodyRecord
  , spShapes      :: IOMap (Record Shape)
  , spConstraints :: IOMap (Record Constraint)
  , spHandlers    :: IOMap (Record CollisionHandler)
  , spacePtr      :: SpacePtr
  }

type instance Elem (Space a) = a

data BodyRecord = BodyRecord
  { brPtr         :: Ptr Body
  , brBody        :: Body
  , brShapes      :: IORef S.IntSet
  , brConstraints :: IORef S.IntSet
  }

data Record a = Record
  { recPtr :: Ptr a
  , recVal :: a
  }

type IOMap a = IORef (M.IntMap a)
type PtrMap a = IOMap (Ptr a)
type SpacePtr = ForeignPtr FrnSpace

-- | Number of iterations per step, global value
newtype Iterations = Iterations Int deriving (Eq, Show)
-- | Gravity force vector, global value
newtype Gravity = Gravity Vec deriving (Eq, Show)
-- | Daming factor, global value
newtype Damping = Damping Double deriving (Eq, Show)
-- | Speed threshold to be considered idle, and a candidate for being put to sleep. Global value.
-- Bodies with a speed less than this will not be simulated until a force acts upon them,
-- which can potentially lead to large gains in performance, especially if there's a lot of
-- inactive bodies in the simulation.
newtype IdleSpeedThreshold = IdleSpeedThreshold Double deriving (Eq, Show)
-- | Sleep idle time threshold, global value
newtype SleepIdleTime = SleepIdleTime Double deriving (Eq, Show)
-- | Collision parameter, global value
newtype CollisionSlop = CollisionSlop Double deriving (Eq, Show)
-- | Collision parameter, global value
newtype CollisionBias = CollisionBias Double deriving (Eq, Show)

cast :: Space a -> Space b
cast (Space b s c h w) = Space b s c h w

-- Constraint subcomponents
newtype MaxForce      = MaxForce      Double deriving (Eq, Show)
newtype MaxBias       = MaxBias       Double deriving (Eq, Show)
newtype ErrorBias     = ErrorBias     Double deriving (Eq, Show)
newtype CollideBodies = CollideBodies Bool   deriving (Eq, Show)
newtype Impulse       = Impulse       Double deriving (Eq, Show)

data Constraint = Constraint Entity Entity ConstraintType deriving (Eq, Show)

data ConstraintType
  = PinJoint BVec BVec -- ^ Maintains a fixed distance between two anchor points
  | SlideJoint BVec BVec Double Double -- ^ A @PinJoint@ with minimum and maximum distance
  | PivotJoint WVec -- ^ Creates a pivot point at the given world coordinate
  | PivotJoint2 BVec BVec -- ^ Creates a pivot point at the given body coordinates
  | GrooveJoint BVec BVec BVec -- ^ The first two vectors are the start and end of the groove on body A, the third argument is the anchor point on body B.
  | DampedSpring BVec BVec Double Double Double -- ^ Spring between two anchor points, with given rest length, stiffness, and damping.
  | DampedRotarySpring Double Double Double -- ^ Rotary sping, with given rest angle, stiffness, and damping.
  | RotaryLimitJoint Double Double -- ^ Joint with minimum and maximum angle
  | RatchetJoint Double Double -- ^ Rathet joint with given phase and ratchet (distance between clicks).
  | GearJoint Double Double -- Keeps angular velocity ratio constant. The first argument is phase, the initial offset, the second argument is the ratio
  | SimpleMotor Double -- ^ Keeps relative angular velocity constant
  deriving (Eq, Show)

-- TODO
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
  -- ^ A callback called when two bodies start touching for the first time.
  -- If it returns 'True', the physics engine will process the collision normally.
  -- If it returns 'False', the physics engine will __ignore the collision entirely__.
  , separateCB  :: Maybe SeparateCB
  -- ^ A callback called when two bodies have just stopped touching. This will
  -- __always__ be called if 'beginCB' is, regardless of the return value of 'beginCB'.
  , preSolveCB  :: Maybe PreSolveCB
  -- ^ A callback called when two bodies are touching during a physics step. If this function
  -- returns 'True', the collision will be processed normally. If it returns 'False, then
    -- the physics engine will stop processing the collision for this step.
  , postSolveCB :: Maybe PostSolveCB
  -- ^ A callback called when two bodies are touching __after__ the response to the collision
  -- has been processed. This means that you can determine the collision impulse or kinetic energy
  -- in this callback, if you need that for processing.
  }

-- | A 'Shape' can have a 'CollisionType'.
--   'CollisionType's are used by callbacks for filtering, also see 'CollisionSource'.
--   The difference between 'CollisionType' and 'CollisionFilter' is that a 'CollisionFilter' determines whether
--   two shapes in the physics engine collide, or pass through one another, whereas a 'CollisionType' determines
--   what callback is called.
--   In general, if you don't want any special checks to happen, use 'CollisionFilter'.
newtype CollisionType = CollisionType C.CUIntPtr
  deriving (Num, Eq, Show)

-- | A 'CollisionSource' determines what types of collisions a callback handles.
--   Also see 'CollisionType'
data CollisionSource
  = Wildcard CollisionType
  | Between CollisionType CollisionType

-- Corresponds to an 'arbiter' in Chipmunk
data Collision = Collision
  { collisionNormal :: Vec
  , collisionBodyA  :: Entity
  , collisionBodyB  :: Entity
  , collisionShapeA :: Entity
  , collisionShapeB :: Entity
  } deriving (Eq, Show)

data CollisionProperties = CollisionProperties
  { collisionElasticity      :: Double
  , collisionFriction        :: Double
  , collisionSurfaceVelocity :: Vec
  } deriving (Eq, Show)

data SegmentQueryResult = SegmentQueryResult
  { sqShape        :: Entity
  -- ^ What entity did this query connect with?
  , sqImpactPoint  :: Vec
  -- ^ The point that the segment impacted with the shape
  , sqImpactNormal :: Vec
  -- ^ The normal of the surface that the segment hit
  , sqImpactAlpha  :: Double
  -- ^ The normalized distance along the query segment in the range `[0, 1]`.
  -- Multiply it by the length of the segment to get the distance away the shape is.
  } deriving (Eq, Show)

data PointQueryResult = PointQueryResult
  { pqShape    :: Entity
  -- ^ What entity did this query connect with?
  , pqPoint    :: WVec
  -- ^ The closest point on the shape's surface (in world space)
  , pqDistance :: Double
  -- ^ The distance to the queried point
  , pqGradient :: Vec
  -- ^ The gradient of the distance function.
  -- This should be similar to 'pqPoint'/'pqDistance' but accurate even for
  -- very small distances.
  } deriving (Eq, Show)
