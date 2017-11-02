{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Linear.V2
import Apecs
import Apecs.Types
import Apecs.TH
import Data.Monoid ((<>))
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

import Context

C.context phycsCtx
C.include "<chipmunk.h>"

instance Store (Space c) where
  type Stores (Space c) = c
  type SafeRW (Space c) = Maybe c
  initStore = do
    (s :: Ptr FrnSpace) <- [C.exp| cpSpace* { cpSpaceNew() } |]
    ref <- newIORef mempty
    return (Space ref (undefined s))

instance Component Position where
  type Storage Position = Space Position

instance Has w Body => Has w Position where
  getStore = (cast :: Space Body -> Space Position) <$> getStore

instance Component Body where
  type Storage Body = Space Body

instance Cast (Space a) (Space b) where
  cast (Space c ref) = Space c ref

