{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Criterion
import qualified Criterion.Main as C

import Control.ECS
import Control.Monad
import Control.DeepSeq

data Position = Position Float Float deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cached (Map Position)

data Velocity = Velocity Float Float deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cached (Map Velocity)

pzero :: Maybe Position
pzero = Just $ Position 0 0
vone :: Maybe Velocity
vone = Just $ Velocity 1 1

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
emptyWorld = liftM3 World (Store <$> newCacheWith 10000 sEmpty)
                          (Store <$> newCacheWith 1000 sEmpty)
                          empty

{-# INLINE stepVelocity #-}
stepVelocity :: Elem (Velocity, Position) -> Writes Position
stepVelocity (Elem (Velocity vx vy, Position px py)
             ) = Writes (Just $ Position (px+vx) (py+vy))

initialize :: System World ()
initialize = do replicateM_ 1000 . newEntityWith $ (Writes (pzero, vone) :: Writes (Position, Velocity))
                replicateM_ 9000 . newEntityWith $ (Writes pzero :: Writes Position)

main :: IO ()
main = C.defaultMain [ bench "init" $ whnfIO (emptyWorld >>= runSystem initialize)
                     , bench "both" $ whnfIO (emptyWorld >>= runSystem (initialize >> mapR stepVelocity))
                     ]
