{-# LANGUAGE Strict, ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Criterion
import Criterion.Types
import qualified Criterion.Main as C
import Control.Monad
import Linear

import Apecs
import Apecs.Stores
import Apecs.Concurrent

-- pos_vel
newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
instance Component ECSPos where type Storage ECSPos = Cache 10000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show)
instance Component ECSVel where type Storage ECSVel = Cache 1000 (Map ECSVel)

makeWorld "PosVel" [''ECSPos, ''ECSVel]

posVelInit = do replicateM_ 1000 (newEntity (ECSPos 0, ECSVel 1))
                replicateM_ 9000 (newEntity (ECSPos 0))

posVelStep = rmap $ \(ECSVel v, ECSPos p) -> ECSPos (p+v)

-- parallel
newtype W1 = W1 Float
newtype W2 = W2 Float
newtype R  = R Float
instance Component W1 where type Storage W1 = Cache 10000 (Map W1)
instance Component W2 where type Storage W2 = Cache 10000 (Map W2)
instance Component R  where type Storage R  = Cache 10000 (Map R)

makeWorld "Parallel" [''W1, ''W2, ''R]

parallelInit, parallelStep :: System Parallel ()
parallelInit = replicateM_ 10000 $ newEntity (W1 0, W2 0, R 0)
parallelStep = do rmap $ \(R x) -> W1 x
                  rmap $ \(R x) -> W2 x

main :: IO ()
main = C.defaultMainWith (C.defaultConfig {timeLimit = 10})
  [ bgroup "pos_vel"
    [ bench "init" $ whnfIO (initPosVel >>= runSystem posVelInit)
    , bench "step" $ whnfIO (initPosVel >>= runSystem (posVelInit >> posVelStep))
    ]
  , bgroup "parallel"
    [ bench "init" $ whnfIO (initParallel >>= runSystem parallelInit)
    , bench "step" $ whnfIO (initParallel >>= runSystem (parallelInit >> parallelStep))
    ]
  ]
