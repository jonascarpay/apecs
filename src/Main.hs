import Control.ECS
import Control.ECS.Immutable

data V2 = V2 !Float !Float
newtype Position = Position (SimpleMap V2)
newtype Velocity = Velocity (SimpleMap V2)

data World = World
  { positions     :: Store Position
  , velocities    :: Store Velocity
  , entityCounter :: Store EntityCounter
  }

main = undefined
