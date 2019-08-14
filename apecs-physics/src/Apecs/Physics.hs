-- | apecs-physics prelude

module Apecs.Physics (

  -- * General
  Physics,

  -- * Space
  Gravity (..), Iterations (..),
  stepPhysics,
  earthGravity,

  -- * Body
  -- $BODY
  Body (..), Position (..), Velocity (..), Angle (..), AngularVelocity (..), Force (..),
  BodyMass (..), Moment (..), CenterOfGravity (..), Torque (..), ShapeList (..), ConstraintList (..),

  -- * Shape
  Convex (..), Shape (..),
  Mass (..), Density (..), Sensor (..), Friction (..), Elasticity (..), SurfaceVelocity (..),
  CollisionFilter (..), CollisionType(..),
  Bitmask (..), maskAll, maskNone, maskList, defaultFilter, boxShape,

  -- * Constraint
  Constraint (..), ConstraintType (..), MaxForce (..), MaxBias (..), ErrorBias (..), Impulse (..), CollideBodies (..),

  -- * Collision
  Collision (..), CollisionHandler (..), defaultHandler,
  CollisionSource(..), BeginCB, SeparateCB, PreSolveCB, PostSolveCB,
  mkBeginCB, mkSeparateCB, mkPreSolveCB, mkPostSolveCB, addPostStepCallback,

  -- * Query
  PointQueryResult (..),
  pointQuery,

  -- * Geometry
  module Apecs.Physics.Geometry,
  BVec, WVec,

  module Apecs,
  module Linear.V2,
  ) where

import           Apecs
import           Linear.V2

import           Apecs.Physics.Body       ()
import           Apecs.Physics.Collision
import           Apecs.Physics.Constraint ()
import           Apecs.Physics.Geometry
import           Apecs.Physics.Query
import           Apecs.Physics.Shape
import           Apecs.Physics.Space
import           Apecs.Physics.Types


-- $BODY
-- When you give an entity a @'Body'@ component in 'apecs-physics', the physics engine will
-- also give this entity a number of __sub-components__.
-- These sub-components may be read and written separately from the actualy @'Body'@ itself,
-- which makes the library both more expressive (as you can only write about the parts of a
-- physics body you actually want to view or change) and more performant
-- (as only the changed parts of a body actually need to be updated when you write to them).
