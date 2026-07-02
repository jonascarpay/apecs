{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.DeepSeq (NFData (..))
import Control.Monad
import qualified Criterion.Main as C
import Criterion.Types
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear

import Apecs
import Apecs.Experimental.ArrayMap
import Apecs.Experimental.ChunkStore
import Apecs.Experimental.SparseStore

-- ---------------------------------------------------------------------------
-- Cache (original ecs-bench baseline)

newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
instance Component ECSPos where type Storage ECSPos = Cache 10000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show)
instance Component ECSVel where type Storage ECSVel = Cache 1000 (Map ECSVel)

makeWorld "PosVel" [''ECSPos, ''ECSVel]
instance NFData PosVel where rnf (PosVel _ _ _) = ()

-- ---------------------------------------------------------------------------
-- Map

newtype MapPos = MapPos (V2 Float)
instance Component MapPos where type Storage MapPos = Map MapPos

newtype MapVel = MapVel (V2 Float)
instance Component MapVel where type Storage MapVel = Map MapVel

makeWorld "MapWorld" [''MapPos, ''MapVel]
instance NFData MapWorld where rnf (MapWorld _ _ _) = ()

-- ---------------------------------------------------------------------------
-- ArrayMapB (boxed)

newtype ArrBPos = ArrBPos (V2 Float)
instance Component ArrBPos where type Storage ArrBPos = ArrayMapB ArrBPos

newtype ArrBVel = ArrBVel (V2 Float)
instance Component ArrBVel where type Storage ArrBVel = ArrayMapB ArrBVel

makeWorld "ArrayBWorld" [''ArrBPos, ''ArrBVel]
instance NFData ArrayBWorld where rnf (ArrayBWorld _ _ _) = ()

-- ---------------------------------------------------------------------------
-- ArrayMapU (unboxed)

newtype ArrUPos = ArrUPos (V2 Float)
$(derivingUnbox "ArrUPos" [t|ArrUPos -> V2 Float|] [|\(ArrUPos v) -> v|] [|ArrUPos|])
instance Component ArrUPos where type Storage ArrUPos = ArrayMapU ArrUPos

newtype ArrUVel = ArrUVel (V2 Float)
$(derivingUnbox "ArrUVel" [t|ArrUVel -> V2 Float|] [|\(ArrUVel v) -> v|] [|ArrUVel|])
instance Component ArrUVel where type Storage ArrUVel = ArrayMapU ArrUVel

makeWorld "ArrayUWorld" [''ArrUPos, ''ArrUVel]
instance NFData ArrayUWorld where rnf (ArrayUWorld _ _ _) = ()

-- ---------------------------------------------------------------------------
-- ChunkStoreB

newtype ChkPos = ChkPos (V2 Float)
instance Component ChkPos where type Storage ChkPos = ChunkStoreB 64 ChkPos

newtype ChkVel = ChkVel (V2 Float)
instance Component ChkVel where type Storage ChkVel = ChunkStoreB 64 ChkVel

makeWorld "ChunkWorld" [''ChkPos, ''ChkVel]
instance NFData ChunkWorld where rnf (ChunkWorld _ _ _) = ()

-- ---------------------------------------------------------------------------
-- SparseStoreB (boxed)

newtype SpBPos = SpBPos (V2 Float)
instance Component SpBPos where type Storage SpBPos = SparseStoreB SpBPos

newtype SpBVel = SpBVel (V2 Float)
instance Component SpBVel where type Storage SpBVel = SparseStoreB SpBVel

makeWorld "SparseBWorld" [''SpBPos, ''SpBVel]
instance NFData SparseBWorld where rnf (SparseBWorld _ _ _) = ()

-- ---------------------------------------------------------------------------
-- SparseStoreU (unboxed)

newtype SpUPos = SpUPos (V2 Float)
$(derivingUnbox "SpUPos" [t|SpUPos -> V2 Float|] [|\(SpUPos v) -> v|] [|SpUPos|])
instance Component SpUPos where type Storage SpUPos = SparseStoreU SpUPos

newtype SpUVel = SpUVel (V2 Float)
$(derivingUnbox "SpUVel" [t|SpUVel -> V2 Float|] [|\(SpUVel v) -> v|] [|SpUVel|])
instance Component SpUVel where type Storage SpUVel = SparseStoreU SpUVel

makeWorld "SparseUWorld" [''SpUPos, ''SpUVel]
instance NFData SparseUWorld where rnf (SparseUWorld _ _ _) = ()

-- ---------------------------------------------------------------------------
-- Systems
--
-- Each variant follows the same workload:
--   init: 1000 entities with (Pos, Vel), 9000 with Pos only  (~10% sparse)
--   step: cmap $ \(Vel v, Pos p) -> Pos (p + v)
--         iterates via Vel (the sparse store), reads Pos, writes Pos

posVelInit :: System PosVel ()
posVelInit = do
  replicateM_ 1000 $ newEntity (ECSPos 0, ECSVel 1)
  replicateM_ 9000 $ newEntity (ECSPos 0)
posVelStep :: System PosVel ()
posVelStep = cmap $ \(ECSVel v, ECSPos p) -> ECSPos (p + v)

mapInit :: System MapWorld ()
mapInit = do
  replicateM_ 1000 $ newEntity (MapPos 0, MapVel 1)
  replicateM_ 9000 $ newEntity (MapPos 0)
mapStep :: System MapWorld ()
mapStep = cmap $ \(MapVel v, MapPos p) -> MapPos (p + v)

arrBInit :: System ArrayBWorld ()
arrBInit = do
  replicateM_ 1000 $ newEntity (ArrBPos 0, ArrBVel 1)
  replicateM_ 9000 $ newEntity (ArrBPos 0)
arrBStep :: System ArrayBWorld ()
arrBStep = cmap $ \(ArrBVel v, ArrBPos p) -> ArrBPos (p + v)

arrUInit :: System ArrayUWorld ()
arrUInit = do
  replicateM_ 1000 $ newEntity (ArrUPos 0, ArrUVel 1)
  replicateM_ 9000 $ newEntity (ArrUPos 0)
arrUStep :: System ArrayUWorld ()
arrUStep = cmap $ \(ArrUVel v, ArrUPos p) -> ArrUPos (p + v)

chkInit :: System ChunkWorld ()
chkInit = do
  replicateM_ 1000 $ newEntity (ChkPos 0, ChkVel 1)
  replicateM_ 9000 $ newEntity (ChkPos 0)
chkStep :: System ChunkWorld ()
chkStep = cmap $ \(ChkVel v, ChkPos p) -> ChkPos (p + v)

spBInit :: System SparseBWorld ()
spBInit = do
  replicateM_ 1000 $ newEntity (SpBPos 0, SpBVel 1)
  replicateM_ 9000 $ newEntity (SpBPos 0)
spBStep :: System SparseBWorld ()
spBStep = cmap $ \(SpBVel v, SpBPos p) -> SpBPos (p + v)

spUInit :: System SparseUWorld ()
spUInit = do
  replicateM_ 1000 $ newEntity (SpUPos 0, SpUVel 1)
  replicateM_ 9000 $ newEntity (SpUPos 0)
spUStep :: System SparseUWorld ()
spUStep = cmap $ \(SpUVel v, SpUPos p) -> SpUPos (p + v)

-- ---------------------------------------------------------------------------

posvelBench :: (NFData w) => String -> IO w -> System w () -> System w () -> C.Benchmark
posvelBench name initW ini step =
  C.bgroup
    name
    [ C.bench "init" $ C.whnfIO (initW >>= runSystem ini)
    , C.bench "step" $
        C.perBatchEnv
          (\_ -> initW >>= \w -> runSystem ini w >> pure w)
          (\w -> runSystem step w)
    ]

main :: IO ()
main =
  C.defaultMainWith
    (C.defaultConfig{timeLimit = 10})
    [ posvelBench "Map" initMapWorld mapInit mapStep
    , posvelBench "Cache" initPosVel posVelInit posVelStep
    , posvelBench "ArrayMapB" initArrayBWorld arrBInit arrBStep
    , posvelBench "ArrayMapU" initArrayUWorld arrUInit arrUStep
    , posvelBench "ChunkStoreB" initChunkWorld chkInit chkStep
    , posvelBench "SparseStoreB" initSparseBWorld spBInit spBStep
    , posvelBench "SparseStoreU" initSparseUWorld spUInit spUStep
    ]
