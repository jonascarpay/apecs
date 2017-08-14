{-# OPTIONS_GHC -fno-warn-unused-binds #-}
import Criterion
import qualified Criterion.Main as C

import Control.ECS
import Control.Monad

data Position = Position Int Int deriving (Eq, Show)
instance Component Position where
  type Storage Position = HashTable Position

data Velocity = Velocity Int Int deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = HashTable Velocity

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
  getC = System $ asks positions

instance World `Has` Velocity where
  getC = System $ asks velocities

instance World `Has` EntityCounter where
  getC = System $ asks entityCounter

emptyWorld :: IO World
emptyWorld = World <$> empty <*> empty <*> empty

stepVelocity :: Reads (Velocity, Position) -> Writes (Position)
stepVelocity (Rd (Just (Velocity vx vy), Just (Position px py))) =
              Wr (Just $ Position (px+vx) (py+vy))

{-stepWorld :: System World ()-}
{-stepWorld = do Store (vs, ps) :: Store (Velocity, Position) <- getC <$> get-}
               {-liftIO . mutForM vs $ \(e, Velocity vx vy) ->-}
                 {-do Just (Position px py) <- mutRetrieve e ps-}
                    {-mutStore e (Just (Position (px+vx) (py+vy))) ps-}

initialize :: System World IO ()
initialize = do replicateM_ 1000 . newEntityWith $ (Wr (pzero, vone) :: Writes (Position, Velocity))
                replicateM_ 9000 . newEntityWith $ (Wr pzero :: Writes Position)


main :: IO ()
main = do w <- emptyWorld
          runWith w $
            do initialize
               e <- nextEntity
               liftIO (print e)

