module Apecs.Physics (

  -- * Global
  Physics,

  -- * Space
  Gravity (..),

  -- * Body
  Body (..), Position (..), Velocity (..), Angle (..), AngularVelocity (..),

  -- * Shape
  ShapeProperties (..), ShapeType (..), Shape (..),
  defaultProperties, defaultFilter,
  hollowBox,

  -- * Constraint
  Constraint (..), ConstraintType (..),

  -- * Collision
  CollisionPair (..), CollisionHandler (..),
  makeCallback,

  module Apecs,
  module Linear.V2,
  ) where

import           Apecs
import           Linear.V2

import           Apecs.Physics.Body       ()
import           Apecs.Physics.Collision
import           Apecs.Physics.Constraint
import           Apecs.Physics.Shape
import           Apecs.Physics.Space      ()
import           Apecs.Physics.Types
