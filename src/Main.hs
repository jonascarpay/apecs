import Criterion
import qualified Criterion.Main as C

import Control.ECS
import Control.Monad.State

data V2 = V2 !Float !Float deriving (Eq, Show)
instance Num V2 where
  V2 x1 y1 + V2 x2 y2 = V2 (x1 + x2) (y1 + y2)

newtype Position = Position V2 deriving (Eq, Show)
instance Component Position where
  type Storage Position = SimpleMap Position

newtype Velocity = Velocity V2 deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = SimpleMap Velocity

veczero = V2 0 0
vecone  = V2 1 1
pzero   = Just $ Position veczero
pone    = Just $ Position vecone
vzero   = Just $ Velocity veczero
vone    = Just $ Velocity vecone

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

addPosition :: V2 -> Reads Position -> Writes Position
addPosition v (Reads (Just (Position p))) = Writes (Just (Position (p+v)))

stepVelocity :: Reads (Velocity, Position) -> Writes (Position)
stepVelocity (Reads (Just (Velocity v), Just (Position p))) =
             Writes (Just (Position (p + v)))

stepWorld :: System World ()
stepWorld = mapReads stepVelocity

main =
  do (_,w :: World) <- runSystemIO initWorld uninitialized
     C.defaultMain [ bench "init" $ whnfIO (defaultMain initWorld)
                   , bench "step" $ whnfIO (runWithIO w stepWorld)
                   , bench "both" $ whnfIO (defaultMain (initWorld >> stepWorld))
                   ]
