{-# LANGUAGE TemplateHaskell #-}

import Control.ECS
import Control.ECS.Immutable
import Control.Lens

data V2 = V2 !Float !Float
newtype Position = Position (SimpleMap V2)
newtype Velocity = Velocity (SimpleMap V2)

data World = World
  { _positions     :: Store Position
  , _velocities    :: Store Velocity
  , _entityCounter :: Store EntityCounter
  }
makeLenses ''World

main = undefined
