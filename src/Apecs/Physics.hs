module Apecs.Physics (

  -- * Global
  Physics,

  -- * Space
  Gravity (..),

  -- * Body
  Body (..), Position (..), Velocity (..), AngularVelocity (..),

  -- * Shape
  ShapeProperties (..), ShapeType (..), Shape (..),
  defaultProperties, defaultFilter,
  hollowBox,

  ) where

import           Apecs.Physics.Body  ()
import           Apecs.Physics.Shape
import           Apecs.Physics.Space ()
import           Apecs.Physics.Types
