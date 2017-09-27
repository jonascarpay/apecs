{-# LANGUAGE Strict, ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Criterion
import qualified Criterion.Main as C
import Control.Monad
import Linear

import Apecs
import Apecs.TH
import Apecs.Stores
import Apecs.Util
import qualified Apecs.Slice as S

-- ecs_bench
newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
instance Component ECSPos where type Storage ECSPos = Cache 10000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show)
instance Component ECSVel where type Storage ECSVel = Cache 1000 (Map ECSVel)

makeWorld "ECSB" [''ECSPos, ''ECSVel]

pvInit = do replicateM_ 1000 (newEntity (ECSPos 0, ECSVel 1))
            replicateM_ 9000 (newEntity (ECSPos 0))

pvStep = rmap $ \(ECSVel v, ECSPos p) -> ECSPos (p+v)

main :: IO ()
main = C.defaultMain
  [ bgroup "ecs_bench"
    [ bench "init" $ whnfIO (initECSB >>= runSystem pvInit)
    , bench "step" $ whnfIO (initECSB >>= runSystem (pvInit >> pvStep))
    ]
  ]
