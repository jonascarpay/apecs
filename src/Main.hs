{-# OPTIONS_GHC -fno-warn-unused-binds #-}
import Criterion
import qualified Criterion.Main as C

import Control.ECS
import Control.ECS.Mutable
import Control.Monad.State

data Position = Position Int Int deriving (Eq, Show)
instance Component Position where
  type Storage Position = MutableMap Position

data Velocity = Velocity Int Int deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = MutableMap Velocity

pzero, pone :: Maybe Position
pzero   = Just $ Position 0 0
pone    = Just $ Position 1 1
vzero, vone :: Maybe Velocity
vzero   = Just $ Velocity 0 0
vone    = Just $ Velocity 1 1

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

initWorld :: System World ()
initWorld = do World <$> empty <*> empty <*> empty >>= put
               replicateM_ 9000 $ newEntityWith (Writes pzero :: Writes Position)
               replicateM_ 1000 $ newEntityWith (Writes (pzero, vone) :: Writes (Position, Velocity))

stepVelocity :: Reads (Velocity, Position) -> Writes (Position)
stepVelocity (Reads (Just (Velocity vx vy), Just (Position px py))) =
  Writes . Just $ Position (px+vx) (py+vy)

stepWorld :: System World ()
stepWorld = mapReads stepVelocity

main :: IO ()
main =
  do (_,w :: World) <- runSystemIO initWorld uninitialized
     C.defaultMain [ bench "init" $ whnfIO (defaultMain initWorld)
                   , bench "step" $ whnfIO (runWithIO w stepWorld)
                   , bench "both" $ whnfIO (defaultMain (initWorld >> stepWorld))
                   ]
