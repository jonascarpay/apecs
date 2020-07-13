module Apecs
  ( -- * Core types
    Entity (..),
    SystemT,
    System,

    -- * Systems
    cmap,
    cmapM_,
    Get,
    Set,
    Destroy,
    Members,
    newEntity,

    -- * Stores
    EntityCounter (..),
    Map (..),
    Global (..),
    Unique (..),
    Cacheable,
    Cache (..),
    ReadOnly (..),
    forceReadonly,

    -- * Misc
    Not (..),
    Initialize,
    Generic,
    Proxy (..),
  )
where

import Apecs.Core
import Apecs.Stores
import Apecs.System
import Data.Proxy
import GHC.Generics (Generic)
