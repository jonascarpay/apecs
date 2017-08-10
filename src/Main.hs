import Control.ECS
import Control.ECS.Immutable

data V2 = V2 !Float !Float
newtype Position = Position (SimpleMap V2)
newtype Velocity = Velocity (SimpleMap V2)

data World = World
  { positions     :: Storage Position
  , velocities    :: Storage Velocity
  , entityCounter :: Storage EntityCounter
  }

main = undefined
