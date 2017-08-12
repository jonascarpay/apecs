{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, TypeOperators #-}

import Control.ECS

data V2 = V2 !Float !Float

newtype Position = Position (SimpleMap V2)

instance Component Position where
  type Storage Position = SimpleMap Position

newtype Velocity = Velocity (SimpleMap V2)

data World = World
  { positions     :: Store Position
  , velocities    :: Store Velocity
  , entityCounter :: Store EntityCounter
  }

instance World `Has` Position where
  getC = positions
  putC p' w = w {positions = p'}

instance World `Has` Velocity where
  getC = velocities
  putC v' w = w {velocities = v'}

instance World `Has` EntityCounter where
  getC = entityCounter
  putC c' w = w {entityCounter = c'}

{-initWorld :: IO World-}
{-initWorld = do p <- empty-}
               {-v <- empty-}
               {-c <- empty-}
               {-return (World p v c)-}

main :: IO ()
main = do --initWorld
          print 1
