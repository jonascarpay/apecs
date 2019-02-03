-- | This module re-exports the apecs prelude with STM versions.

module Apecs.STM.Prelude
  ( System
  , module Apecs.STM
  , module Apecs
  ) where

import           Apecs     hiding (EntityCounter, Global, Map, System, Unique,
                            makeWorld, newEntity)
import           Apecs.STM

type System w a = SystemT w STM a
