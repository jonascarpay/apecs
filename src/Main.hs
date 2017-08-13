import Control.ECS
import Control.Monad.State

data V2 = V2 !Float !Float deriving (Eq, Show)

veczero = V2 0 0
pzero = Just $ Position veczero
vzero = Just $ Velocity veczero
czero = (pzero, vzero)

newtype Position = Position V2 deriving (Eq, Show)
instance Component Position where
  type Storage Position = SimpleMap Position

newtype Velocity = Velocity V2 deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = SimpleMap Velocity

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
initWorld = World <$> empty <*> empty <*> empty >>= put

main :: IO ()
main = defaultMain $ do initWorld
                        newEntityWith (Writes pzero :: Writes Position)
                        newEntityWith (Writes vzero :: Writes Velocity)
                        e' <- newEntityWith (Writes czero :: Writes (Position, Velocity))
                        sl1 :: Slice Position <- embed $ slice
                        sl2 :: Slice Velocity <- embed $ slice
                        liftIO $ print sl1
                        liftIO $ print sl2

