{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Apecs.Physics.Space where

import           Apecs
import           Apecs.Types
import           Data.IORef
import           Foreign.Concurrent
import           Foreign.ForeignPtr  (withForeignPtr)
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Types

C.context phycsCtx
C.include "<chipmunk.h>"

-- Space
newSpace :: IO SpacePtr
newSpace = do
    spaceRaw <- [C.exp| cpSpace* { cpSpaceNew() } |]
    newForeignPtr spaceRaw $ do
      error "deallocate other objects before removing the space"
      [C.exp| void { cpSpaceFree($(cpSpace* spaceRaw)) } |] -- FIXME: deallocate all map entries

stepPhysics :: SpacePtr -> Double -> IO ()
stepPhysics spacePtr (realToFrac -> dT) = withForeignPtr spacePtr $ \space ->
  [C.exp| void { cpSpaceStep( $(cpSpace* space), $(double dT) ) } |]

stepPhysicsSys dT = do
  Space _ spacePtr :: Space Physics <- getStore
  liftIO$ stepPhysics spacePtr dT

defaultSetMaybe s ety Nothing  = explDestroy s ety
defaultSetMaybe s ety (Just x) = explSet s ety x

instance Component Physics where
  type Storage Physics = Space Physics

instance Store (Space Physics) where
  type Stores (Space Physics) = Physics
  type SafeRW (Space Physics) = Physics
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

-- Gravity
getGravity :: SpacePtr -> IO (V2 Double)
getGravity spacePtr = withForeignPtr spacePtr $ \space -> do
  x <- [C.exp| double { cpSpaceGetGravity ($(cpSpace* space)).x } |]
  y <- [C.exp| double { cpSpaceGetGravity ($(cpSpace* space)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setGravity :: SpacePtr -> V2 Double -> IO ()
setGravity spacePtr (V2 (realToFrac -> x) (realToFrac -> y)) = withForeignPtr spacePtr $ \space -> [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpSpaceSetGravity($(cpSpace* space), vec);
  } |]

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

