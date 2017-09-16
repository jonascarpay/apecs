{-# LANGUAGE FlexibleContexts #-}

module Apecs (
  -- Types
    System(..), runSystem, runWith,
    Component(..), Entity(..), Slice, Has(..), Safe(..), cast,

    -- Initializable
    initStoreWith,

    -- HasMembers
    destroy, exists, owners, resetStore,

    -- Store
    get, set, setMaybe, modify,
    cmap, cmapM, cmapM_, cimapM, cimapM_,
    sliceSize,

    -- GlobalRW
    readGlobal, writeGlobal, modifyGlobal,

    -- Query
    slice, All(..),

  -- Reader
  asks, ask, liftIO, lift,
) where

import Control.Monad.Reader (asks, ask, liftIO, lift)

import Apecs.Types
import Apecs.System

