{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import           Control.Monad
import           Criterion
import qualified Criterion.Main  as C
import           Linear
import           Data.Typeable (Typeable, showsTypeRep, typeRep)

import           Apecs

newtype ECSPos = ECSPos (V2 Float) deriving (Typeable, Eq, Show)
instance Component ECSPos where type Storage ECSPos = Map ECSPos

newtype ECSVel = ECSVel (V2 Float) deriving (Typeable, Eq, Show)
instance Component ECSVel where type Storage ECSVel = Map ECSVel

makeWorld "PosVel" [''ECSPos, ''ECSVel]

deriving instance Typeable PosVel

posVelInit :: Int -> System PosVel ()
posVelInit n = do
  replicateM_ n $ newEntity (ECSPos 0, ECSVel 1)
  replicateM_ (9 * n) $ newEntity (ECSPos 0)

posVelStep :: System PosVel ()
posVelStep = cmap $ \(ECSVel v, ECSPos p) -> ECSPos (p+v)

newtype CachedECSPos = CachedECSPos (V2 Float) deriving (Typeable, Eq, Show)
instance Component CachedECSPos where type Storage CachedECSPos = Cache 10_000 (Map CachedECSPos)

newtype CachedECSVel = CachedECSVel (V2 Float) deriving (Typeable, Eq, Show)
instance Component CachedECSVel where type Storage CachedECSVel = Cache 10_000 (Map CachedECSVel)

makeWorld "CachedPosVel" [''CachedECSPos, ''CachedECSVel]

deriving instance Typeable CachedPosVel

cachedPosVelInit :: Int -> System CachedPosVel ()
cachedPosVelInit n = do
  replicateM_ n $ newEntity (CachedECSPos 0, CachedECSVel 1)
  replicateM_ (9 * n) $ newEntity (CachedECSPos 0)

cachedPosVelStep :: System CachedPosVel ()
cachedPosVelStep = cmap $ \(CachedECSVel v, CachedECSPos p) -> CachedECSPos (p+v)

benchWith :: forall w. (Typeable w) => IO w -> (Int -> System w ()) -> System w () -> Benchmark
benchWith initWorld initEntities stepEntities = 
  let gName = showsTypeRep (typeRep (Proxy :: Proxy w)) ""
      n = 10_000
   in bgroup gName
        [ bench "init world" $ whnfIO (initWorld >>= runSystem (pure ()))
        , bench "init" $ whnfIO (initWorld >>= runSystem (initEntities n))
        , bench "step" $ whnfIO (initWorld >>= runSystem (initEntities n >> replicateM_ 1_000 stepEntities))
        ]

main :: IO ()
main = C.defaultMain --With (C.defaultConfig {timeLimit = 99})
  [ benchWith initPosVel posVelInit posVelStep
  , benchWith initCachedPosVel cachedPosVelInit cachedPosVelStep
  ]

