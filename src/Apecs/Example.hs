{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Apecs.Example (main) where

import Apecs.Core (Get, Initialize (..))
import Apecs.Stores (EntityCounter, Map, newEntity)
import Apecs.System (cmap, cmapM_)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.State (evalStateT)
import GHC.Generics (Generic)

newtype Position = Position Float deriving (Eq, Show)

newtype Velocity = Velocity Float deriving (Eq, Show)

data World
  = World
      (Map Position)
      (Map Velocity)
      EntityCounter
  deriving (Generic, Initialize IO)

main :: IO ()
main = do
  (w :: World) <- initialize
  flip evalStateT w $ do
    newEntity (Position 0, Velocity 3)
    newEntity (Position 1, Velocity 4)
    newEntity (Position 2, Velocity 5)
    cmap $ \(Position p, Velocity v) -> Position (p + v)
    cmapM_ $ \(Position p) -> liftIO $ print p

-- Custom composite component
data Kinetic = Kinetic
  { pos :: Position,
    vel :: Velocity
  }
  deriving (Eq, Show, Generic, Get World IO)
