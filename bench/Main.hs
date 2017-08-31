{-# LANGUAGE DataKinds, BangPatterns, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Criterion
import qualified Criterion.Main as C
import Control.Monad

import Control.ECS
import Control.ECS.Vector
import Control.ECS.Storage.Mutable
import Control.ECS.Storage.Immutable

data Position = Position (V2 Float) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cache 10000 (Map Position)

data Velocity = Velocity (V2 Float) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cache 1000 (Map Velocity)

data World = World
  { positions     :: Store Position
  , velocities    :: Store Velocity
  , entityCounter :: Store EntityCounter
  }

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
emptyWorld = liftM3 World empty empty empty

{-# INLINE stepVelocity #-}
stepVelocity :: Elem (Velocity, Position) -> Writes Position
stepVelocity (Elem (Velocity !v, Position !p)
             ) = Writes (Just $ Position (p+v))

initialize :: System World ()
initialize = do replicateM_ 1000 . newEntityWith $ (Elem (Position 0, Velocity 1) :: Elem (Position, Velocity))
                replicateM_ 9000 . newEntityWith $ (Elem (Position 0) :: Elem Position)

main :: IO ()
main = C.defaultMain [ bench "init" $ whnfIO (emptyWorld >>= runSystem initialize)
                     , bench "both" $ whnfIO (emptyWorld >>= runSystem (initialize >> apply stepVelocity))
                     ]
