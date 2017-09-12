{-# LANGUAGE Strict, ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

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
  getStore = System $ asks positions

instance World `Has` Velocity where
  getStore = System $ asks velocities

instance World `Has` EntityCounter where
  getStore = System $ asks entityCounter

emptyWorld :: IO World
emptyWorld = liftM3 World initStore initStore initCounter

cStep (Velocity v, Position p) = (Velocity v, Position (p+v))
rStep (Velocity v, Position p) = Position (p+v)

rStep' :: (Velocity, Position) -> Safe Position
rStep' (Velocity v, Position p) = Safe (Just (Position (p+v)))

wStep' :: Safe (Velocity, Position) -> Safe Position
wStep' (Safe (Just (Velocity v), Just (Position p))) = Safe (Just (Position (p+v)))

wStep :: Safe (Velocity, Position) -> Position
wStep (Safe (Just (Velocity v), Just (Position p))) = Position (p+v)

explicit = do sl :: Slice (Velocity, Position) <- owners
              forMC_ sl $ \(e,Safe (Just (Velocity v), Just (Position p))) -> set (cast e) (Position $ p + v)

cStep1 (Velocity p) = (Velocity (p+1))

initialize :: System World ()
initialize = do replicateM_ 1000 $ newEntity (Position 0, Velocity 1)
                replicateM_ 9000 $ newEntity (Position 0)

main :: IO ()
main = C.defaultMain [ bench "init" $ whnfIO (emptyWorld >>= runSystem initialize)
                     , bgroup "init and step"
                       [ bench "cmap"   $ whnfIO (emptyWorld >>= runSystem (initialize >> cmap  cStep))
                       , bench "cmap1"  $ whnfIO (emptyWorld >>= runSystem (initialize >> cmap  cStep1))
                       , bench "rmap"   $ whnfIO (emptyWorld >>= runSystem (initialize >> rmap  rStep))
                       , bench "rmap'"  $ whnfIO (emptyWorld >>= runSystem (initialize >> rmap' rStep'))
                       , bench "wmap"   $ whnfIO (emptyWorld >>= runSystem (initialize >> wmap  wStep))
                       , bench "wmap'"  $ whnfIO (emptyWorld >>= runSystem (initialize >> wmap' wStep'))
                       , bench "forMC_" $ whnfIO (emptyWorld >>= runSystem (initialize >> explicit))
                       ]
                     ]
