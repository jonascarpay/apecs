import Control.ECS
import Control.ECS.Immutable

data V2 = V2 !Float !Float
newtype Position = Position (SimpleMap V2)
newtype Velocity = Velocity (SimpleMap V2)
newtype IsEnemy  = IsEnemy SimpleFlag

data World = World
  { positions     :: Storage Position
  , velocities    :: Storage Velocity
  , enemies       :: Storage IsEnemy
  , entityCounter :: Storage EntityCounter
  }

main = undefined
