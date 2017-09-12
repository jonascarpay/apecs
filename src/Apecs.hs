{-# LANGUAGE FlexibleContexts #-}

module Apecs (
  -- Core
    System(..), runSystem, runWith,
    Component(..), Entity, Slice, Has(..), Safe(..), cast,

    -- Initializable
    initStoreWith,

    -- HasMembers
    destroy, exists, owners,

    -- Store
    get, set, setMaybe, modify,
    cmap, cmapM, cmapM_, cimapM, cimapM_,
    sliceSize,

    -- GlobalRW
    readGlobal, writeGlobal, modifyGlobal,

    -- Query
    slice, All(..),

    -- Slices
    A.forM, A.forMC, A.forM_, A.forMC_, A.mapM, A.mapMC, A.mapM_, A.mapMC_,

  -- Reader
  asks, ask, liftIO, lift,
) where

import Apecs.Core as A
import Control.Monad.Reader (asks, ask, liftIO, lift)
