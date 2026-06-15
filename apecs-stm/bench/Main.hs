{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

{-| Benchmarks the same pos/vel init+step workload across three worlds:

  * @apecs-stm@: STM stores and the STM 'STM.newEntity' (run atomically).
  * vanilla @apecs@: 'Map' stores and the default 'newEntity'
    (backed by the non-atomic 'nextEntity').
  * vanilla @apecs@ using 'nextEntityIO': same as above, but allocating
    entities through the atomic 'nextEntityIO' counter path.
-}
module Main (main) where

import Control.Monad (replicateM_, void)
import Linear (V2)
import Test.Tasty.Bench

import Apecs
import qualified Apecs.STM as STM
import Apecs.Util (nextEntityIO)

-- Vanilla world -------------------------------------------------------------

newtype Pos = Pos (V2 Float) deriving (Eq, Show)
instance Component Pos where type Storage Pos = Map Pos

newtype Vel = Vel (V2 Float) deriving (Eq, Show)
instance Component Vel where type Storage Vel = Map Vel

makeWorld "Vanilla" [''Pos, ''Vel]

-- STM world -----------------------------------------------------------------

newtype SPos = SPos (V2 Float) deriving (Eq, Show)
instance Component SPos where type Storage SPos = STM.Map SPos

newtype SVel = SVel (V2 Float) deriving (Eq, Show)
instance Component SVel where type Storage SVel = STM.Map SVel

STM.makeWorld "Stm" [''SPos, ''SVel]

-- Workloads -----------------------------------------------------------------

-- | Allocate an entity via the atomic IO counter, mirroring 'newEntity'.
newEntityIO :: (Set w IO c, Has w IO EntityCounter) => c -> SystemT w IO Entity
newEntityIO c = do
  ety <- nextEntityIO
  set ety c
  return ety

vanillaInit :: System Vanilla ()
vanillaInit = do
  replicateM_ 1000 $ void $ newEntity (Pos 0, Vel 1)
  replicateM_ 9000 $ void $ newEntity (Pos 0)

vanillaInitIO :: System Vanilla ()
vanillaInitIO = do
  replicateM_ 1000 $ void $ newEntityIO (Pos 0, Vel 1)
  replicateM_ 9000 $ void $ newEntityIO (Pos 0)

vanillaStep :: System Vanilla ()
vanillaStep = cmap $ \(Vel v, Pos p) -> Pos (p + v)

stmInit :: SystemT Stm STM.STM ()
stmInit = do
  replicateM_ 1000 $ void $ STM.newEntity (SPos 0, SVel 1)
  replicateM_ 9000 $ void $ STM.newEntity (SPos 0)

stmStep :: SystemT Stm STM.STM ()
stmStep = cmap $ \(SVel v, SPos p) -> SPos (p + v)

{- | Tasty pattern selecting the @vanilla@ benchmark within the given group,
used as the reference point for 'bcompare'.
-}
vsVanilla :: String -> String
vsVanilla grp = "$(NF-1) == \"" ++ grp ++ "\" && $NF == \"vanilla\""

main :: IO ()
main =
  defaultMain
    [ bgroup
        "init"
        [ bench "vanilla" $ whnfIO (initVanilla >>= runSystem vanillaInit)
        , bcompare (vsVanilla "init") $
            bench "vanilla-nextEntityIO" $
              whnfIO (initVanilla >>= runSystem vanillaInitIO)
        , bcompare (vsVanilla "init") $
            bench "apecs-stm" $
              whnfIO (initStm >>= runSystem (STM.atomically stmInit))
        ]
    , bgroup
        "step"
        [ bench "vanilla" $ whnfIO (initVanilla >>= runSystem (vanillaInit >> vanillaStep))
        , bcompare (vsVanilla "step") $
            bench "vanilla-nextEntityIO" $
              whnfIO (initVanilla >>= runSystem (vanillaInitIO >> vanillaStep))
        , bcompare (vsVanilla "step") $
            bench "apecs-stm" $
              whnfIO (initStm >>= runSystem (STM.atomically (stmInit >> stmStep)))
        ]
    ]
