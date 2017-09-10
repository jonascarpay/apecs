{-# LANGUAGE ScopedTypeVariables, DataKinds, BangPatterns, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Criterion
import qualified Criterion.Main as C
import Control.Monad

import Apecs as A
import Apecs.Stores
import Apecs.Util
import Apecs.Vector

newtype Position = Position (V2 Float) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cache 10000 (Map Position)

newtype Velocity = Velocity (V2 Float) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cache 1000 (Map Velocity)

data World = World
  { positions     :: Storage Position
  , velocities    :: Storage Velocity
  , entityCounter :: Storage EntityCounter
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
emptyWorld = liftM3 World (initStore ()) (initStore ()) initCounter

{-# INLINE stepVelocity #-}
stepVelocity (Velocity !v, Position !p) = Position (p+v)

explicit :: System World ()
explicit = do sl :: Slice (Velocity, Position) <- sliceAll
              sliceForM_ sl $ \e -> do
                Safe (Just (Velocity v), Just (Position p)) <- get e
                set (cast e) (Position $ p + v)

initialize :: System World ()
initialize = do replicateM_ 1000 $ newEntity (Position 0, Velocity 1)
                replicateM_ 9000 $ newEntity (Position 0)

main :: IO ()
main = C.defaultMain [ bench "init" $ whnfIO (emptyWorld >>= runSystem initialize)
                     , bgroup "init >> stepVelocity"
                       [ bench "apply stepVelocity"    $ whnfIO (emptyWorld >>= runSystem (initialize >> rmap' stepVelocity))
                       , bench "explicit stepVelocity" $ whnfIO (emptyWorld >>= runSystem (initialize >> explicit))
                       ]
                     ]
