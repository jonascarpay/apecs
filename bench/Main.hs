{-# LANGUAGE Strict, ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Criterion
import Criterion.Types
import qualified Criterion.Main as C
import Control.Monad
import Linear

import Apecs

-- pos_vel
newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
instance Component ECSPos where type Storage ECSPos = Cache 10000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show)
instance Component ECSVel where type Storage ECSVel = Cache 1000 (Map ECSVel)

makeWorld "PosVel" [''ECSPos, ''ECSVel]

posVelInit :: System PosVel ()
posVelInit = do replicateM_ 1000 (newEntity (ECSPos 0, ECSVel 1))
                replicateM_ 9000 (newEntity (ECSPos 0))

posVelStep :: System PosVel ()
posVelStep = cmap $ \(ECSVel v, ECSPos p) -> ECSPos (p+v)

main :: IO ()
main = C.defaultMainWith (C.defaultConfig {timeLimit = 10})
  [ bgroup "pos_vel"
    [ bench "init" $ whnfIO (initPosVel >>= runSystem posVelInit)
    , bench "step" $ whnfIO (initPosVel >>= runSystem (posVelInit >> posVelStep))
    ]
  ]
