{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Instances where

import           Apecs
import           Apecs.TH
import           Apecs.Types
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Map            as Map
import           Data.Monoid         ((<>))
import qualified Data.Vector.Unboxed as U
import           Foreign.Concurrent
import           Foreign.ForeignPtr  (ForeignPtr, withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import qualified Language.C.Types    as C
import qualified Language.Haskell.TH as TH
import           Linear.V2

import           Types
import           Wrapper

C.context phycsCtx
C.include "<chipmunk.h>"

stepPhysicsSys dT = do
  Space _ spacePtr :: Space Physics <- getStore
  liftIO$ stepPhysics spacePtr dT

defaultSetMaybe s ety Nothing  = explDestroy s ety
defaultSetMaybe s ety (Just x) = explSet s ety x

instance Component Physics where
  type Storage Physics = Space Physics

instance Store (Space Physics) where
  type Stores (Space Physics) = Physics
  type Stores (Space Physics) = Physics
  initStore = do
    spacePtr <- newSpace
    mapRef   <- newIORef mempty
    return (Space mapRef spacePtr)

  explSet _ _ _ = return ()
  explGet _ _ = return (error "Can't produce a Physics")
  explDestroy _ _ = return ()
  explMembers _ = return mempty
  explExists _ _ = return False
  explGetUnsafe _ _ = return (error "Can't produce a Physics")
  explSetMaybe _ _ _ = return ()

instance Component Gravity where
  type Storage Gravity = Space Gravity

instance Has w Physics => Has w Gravity where
  getStore = (cast :: Space Physics -> Space Gravity) <$> getStore

instance GlobalStore (Space Gravity)

instance Store (Space Gravity) where
  type Stores (Space Gravity) = Gravity
  type SafeRW (Space Gravity) = Gravity
  initStore = error "Initializing space from non-Physics store"
  explDestroy _ _ = return ()
  explMembers _   = return mempty
  explExists _ _  = return False
  explSet (Space mapRef spcPtr) _ (Gravity v) = setGravity spcPtr v
  explGet (Space mapRef spcPtr) _ = Gravity <$> getGravity spcPtr
  explSetMaybe  = explSet
  explGetUnsafe = explGet



