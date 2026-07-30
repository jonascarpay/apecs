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

-- pos_vel
newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
derivingUnbox "ECSPos" [t|ECSPos -> V2 Float|] [|\(ECSPos v) -> v|] [|ECSPos|]

instance Component ECSPos where type Storage ECSPos = UCache 10000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show, Storable)
instance Component ECSVel where type Storage ECSVel = SCache 1000 (Map ECSVel)

makeWorld "PosVel" [''ECSPos, ''ECSVel]
instance NFData PosVel where rnf PosVel{} = ()

posVelInit :: System PosVel ()
posVelInit = do
  replicateM_ 1000 $ newEntity (ECSPos 0, ECSVel 1)
  replicateM_ 9000 $ newEntity (ECSPos 0)

posVelStep :: System PosVel ()
posVelStep = cmap $ \(ECSVel v, ECSPos p) -> ECSPos (p + v)

main :: IO ()
main =
  C.defaultMainWith
    (C.defaultConfig{timeLimit = 10})
    [ bgroup
        "pos_vel"
        [ bench "init" $ whnfIO (initPosVel >>= runSystem posVelInit)
        , bench "step" $
            perBatchEnv
              (\_ -> initPosVel >>= \w -> runSystem posVelInit w >> pure w)
              (runSystem posVelStep)
        ]
    ]
