{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.DeepSeq (NFData (..))
import Control.Monad
import Criterion
import qualified Criterion.Main as C
import Criterion.Types
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear

import Apecs
import Foreign (Storable)

-- pos_vel, uncached
newtype Pos = Pos (V2 Float) deriving (Eq, Show)
instance Component Pos where type Storage Pos = Map Pos

newtype Vel = Vel (V2 Float) deriving (Eq, Show)
instance Component Vel where type Storage Vel = Map Vel

-- pos_vel, boxed cache
newtype BPos = BPos (V2 Float) deriving (Eq, Show)
instance Component BPos where type Storage BPos = Cache 10000 (Map BPos)

newtype BVel = BVel (V2 Float) deriving (Eq, Show)
instance Component BVel where type Storage BVel = Cache 1000 (Map BVel)

-- pos_vel, storable cache
newtype SPos = SPos (V2 Float) deriving (Eq, Show, Storable)
instance Component SPos where type Storage SPos = SCache 10000 (Map SPos)

newtype SVel = SVel (V2 Float) deriving (Eq, Show, Storable)
instance Component SVel where type Storage SVel = SCache 1000 (Map SVel)

-- pos_vel, unboxed cache
newtype UPos = UPos (V2 Float) deriving (Eq, Show)
derivingUnbox "UPos" [t|UPos -> V2 Float|] [|\(UPos v) -> v|] [|UPos|]
instance Component UPos where type Storage UPos = UCache 10000 (Map UPos)

newtype UVel = UVel (V2 Float) deriving (Eq, Show)
derivingUnbox "UVel" [t|UVel -> V2 Float|] [|\(UVel v) -> v|] [|UVel|]
instance Component UVel where type Storage UVel = UCache 1000 (Map UVel)

makeWorld "PosVel" [''MPos, ''MVel, ''BPos, ''BVel, ''SPos, ''SVel, ''UPos, ''UVel]
instance NFData PosVel where rnf PosVel{} = ()

rawInit :: System PosVel ()
rawInit = do
  replicateM_ 1000 $ newEntity (Pos 0, Vel 1)
  replicateM_ 9000 $ newEntity (Pos 0)

rawStep :: System PosVel ()
rawStep = cmap $ \(Vel v, Pos p) -> Pos (p + v)

boxedInit :: System PosVel ()
boxedInit = do
  replicateM_ 1000 $ newEntity (BPos 0, BVel 1)
  replicateM_ 9000 $ newEntity (BPos 0)

boxedStep :: System PosVel ()
boxedStep = cmap $ \(BVel v, BPos p) -> BPos (p + v)

storableInit :: System PosVel ()
storableInit = do
  replicateM_ 1000 $ newEntity (SPos 0, SVel 1)
  replicateM_ 9000 $ newEntity (SPos 0)

storableStep :: System PosVel ()
storableStep = cmap $ \(SVel v, SPos p) -> SPos (p + v)

unboxedInit :: System PosVel ()
unboxedInit = do
  replicateM_ 1000 $ newEntity (UPos 0, UVel 1)
  replicateM_ 9000 $ newEntity (UPos 0)

unboxedStep :: System PosVel ()
unboxedStep = cmap $ \(UVel v, UPos p) -> UPos (p + v)

posVelGroup :: String -> System PosVel () -> System PosVel () -> Benchmark
posVelGroup name initSys stepSys =
  bgroup
    name
    [ bench "init" $ whnfIO (initPosVel >>= runSystem initSys)
    , bench "step" $
        perBatchEnv
          (\_ -> initPosVel >>= \w -> runSystem initSys w >> pure w)
          (runSystem stepSys)
    ]

main :: IO ()
main =
  C.defaultMainWith
    (C.defaultConfig{timeLimit = 10})
    [ bgroup
        "pos_vel"
        [ posVelGroup "raw" rawInit rawStep
        , posVelGroup "boxed" boxedInit boxedStep
        , posVelGroup "storable" storableInit storableStep
        , posVelGroup "unboxed" unboxedInit unboxedStep
        ]
    ]
