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
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main (main) where

import           Control.Monad
import           Criterion
import qualified Criterion.Main  as C
import           Foreign.Storable (Storable)
import           Linear
import           Data.Typeable (Typeable, showsTypeRep, typeRep)
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU

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

newtype PreallocatedECSPos = PreallocatedECSPos (V2 Float) deriving (Typeable, Eq, Show)
instance Component PreallocatedECSPos where type Storage PreallocatedECSPos = MapWith 5_000 PreallocatedECSPos

newtype PreallocatedECSVel = PreallocatedECSVel (V2 Float) deriving (Typeable, Eq, Show)
instance Component PreallocatedECSVel where type Storage PreallocatedECSVel = MapWith 5_000 PreallocatedECSVel

makeWorld "PreallocatedPosVel" [''PreallocatedECSPos, ''PreallocatedECSVel]

deriving instance Typeable PreallocatedPosVel

preallocatedPosVelInit :: Int -> System PreallocatedPosVel ()
preallocatedPosVelInit n = do
  replicateM_ n $ newEntity (PreallocatedECSPos 0, PreallocatedECSVel 1)
  replicateM_ (9 * n) $ newEntity (PreallocatedECSPos 0)

preallocatedPosVelStep :: System PreallocatedPosVel ()
preallocatedPosVelStep = cmap $ \(PreallocatedECSVel v, PreallocatedECSPos p) -> PreallocatedECSPos (p+v)

newtype UECSPos = UECSPos (V2 Float) deriving (Typeable, Eq, Show)
instance Component UECSPos where type Storage UECSPos = UMap UECSPos

newtype instance VU.MVector s UECSPos = MV_UECSPos (VU.MVector s (V2 Float))
newtype instance VU.Vector    UECSPos = V_UECSPos  (VU.Vector    (V2 Float))
deriving instance VGM.MVector VU.MVector UECSPos
deriving instance VG.Vector   VU.Vector  UECSPos
instance VU.Unbox UECSPos

newtype UECSVel = UECSVel (V2 Float) deriving (Typeable, Eq, Show)
instance Component UECSVel where type Storage UECSVel = UMap UECSVel

newtype instance VU.MVector s UECSVel = MV_UECSVel (VU.MVector s (V2 Float))
newtype instance VU.Vector    UECSVel = V_UECSVel  (VU.Vector    (V2 Float))
deriving instance VGM.MVector VU.MVector UECSVel
deriving instance VG.Vector   VU.Vector  UECSVel
instance VU.Unbox UECSVel

makeWorld "UPosVel" [''UECSPos, ''UECSVel]

deriving instance Typeable UPosVel

uPosVelInit :: Int -> System UPosVel ()
uPosVelInit n = do
  replicateM_ n $ newEntity (UECSPos 0, UECSVel 1)
  replicateM_ (9 * n) $ newEntity (UECSPos 0)

uPosVelStep :: System UPosVel ()
uPosVelStep = cmap $ \(UECSVel v, UECSPos p) -> UECSPos (p+v)

newtype SECSPos = SECSPos (V2 Float) deriving (Typeable, Storable, Eq, Show)
instance Component SECSPos where type Storage SECSPos = SMap SECSPos

newtype SECSVel = SECSVel (V2 Float) deriving (Typeable, Storable, Eq, Show)
instance Component SECSVel where type Storage SECSVel = SMap SECSVel

makeWorld "SPosVel" [''SECSPos, ''SECSVel]

deriving instance Typeable SPosVel

sPosVelInit :: Int -> System SPosVel ()
sPosVelInit n = do
  replicateM_ n $ newEntity (SECSPos 0, SECSVel 1)
  replicateM_ (9 * n) $ newEntity (SECSPos 0)

sPosVelStep :: System SPosVel ()
sPosVelStep = cmap $ \(SECSVel v, SECSPos p) -> SECSPos (p+v)

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
main = C.defaultMain
  [ benchWith initPosVel posVelInit posVelStep
  , benchWith initCachedPosVel cachedPosVelInit cachedPosVelStep
  , benchWith initPreallocatedPosVel preallocatedPosVelInit preallocatedPosVelStep
  , benchWith initUPosVel uPosVelInit uPosVelStep
  , benchWith initSPosVel sPosVelInit sPosVelStep
  ]

