-- | Phycs prelude

module Apecs.Physics (

  -- * Global
  Physics,

  -- * Space
  Gravity (..),

  -- * Body
  Body (..), Position (..), Velocity (..), Angle (..), AngularVelocity (..), Force (..),
  Mass (..), Moment (..), CenterOfGravity (..), Torque (..),

  -- * Shape
  ShapeProperties (..), ShapeType (..), Shape (..), Mass, Density, Sensor, Friction, Elasticity, SurfaceVelocity, CollisionFilter,
  defaultProperties, defaultFilter,
  hollowBox, box,

  -- * Constraint
  Constraint (..), ConstraintType (..), MaxForce (..), MaxBias (..), ErrorBias (..), CollideBodies (..),

  -- * Collision
  Collision (..), CollisionHandler (..), defaultHandler,
  CollisionSource(..), BeginCB, SeparateCB, PreSolveCB, PostSolveCB,
  mkBeginCB, mkSeparateCB, mkPreSolveCB, mkPostSolveCB,

  module Apecs,
  module Linear.V2,
  ) where

import           Apecs
import           Linear.V2

import           Apecs.Physics.Body       ()
import           Apecs.Physics.Collision
import           Apecs.Physics.Constraint ()
import           Apecs.Physics.Shape
import           Apecs.Physics.Space      ()
import           Apecs.Physics.Types
