{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Criterion
import qualified Criterion.Main as C

import Control.ECS
import Control.ECS.Storage.Immutable
import Control.Monad
import Control.DeepSeq

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

instance NFData World where
  rnf w = seq w ()

instance World `Has` Position where
  {-# INLINE getStore #-}
  getStore = System $ asks positions

instance World `Has` Velocity where
  {-# INLINE getStore #-}
  getStore = System $ asks velocities

instance World `Has` EntityCounter where
  {-# INLINE getStore #-}
  getStore = System $ asks entityCounter

emptyWorld :: IO World
emptyWorld = World <$> empty <*> empty <*> empty

{-# INLINE stepVelocity #-}
stepVelocity :: Reads (Velocity, Position) -> Writes Position
stepVelocity (Reads ( Just (Velocity vx vy)
                    , Just (Position px py))
             ) = Writes (Just $ Position (px+vx) (py+vy))

initialize :: System World IO ()
initialize = do replicateM_ 1000 . newEntityWith $ (Writes (pzero, vone) :: Writes (Position, Velocity))
                replicateM_ 9000 . newEntityWith $ (Writes pzero :: Writes Position)

main :: IO ()
main = C.defaultMain [ bench "init" $ whnfIO (emptyWorld >>= runSystem initialize)
                     , env (do w <- emptyWorld; runSystem initialize w; return w)
                               (\w -> bench "step" $ whnfIO (runSystem (mapReads stepVelocity) w))
                     ]

