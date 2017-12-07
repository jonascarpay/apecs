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
    newForeignPtr spaceRaw [C.exp| void { cpSpaceFree($(cpSpace* spaceRaw)) } |]

explStepPhysics :: SpacePtr -> Double -> IO ()
explStepPhysics spacePtr (realToFrac -> dT) = withForeignPtr spacePtr $ \space ->
  [C.exp| void { cpSpaceStep( $(cpSpace* space), $(double dT) ) } |]

stepPhysics :: Has w Physics => Double -> System w ()
stepPhysics dT = do
  s :: Space Physics <- getStore
  liftIO$ explStepPhysics (spacePtr s) dT

instance Component Physics where
  type Storage Physics = Space Physics

instance Store (Space Physics) where
  type Stores (Space Physics) = Physics
  type SafeRW (Space Physics) = ()
  initStore = do
    spacePtr <- newSpace
    bRef     <- newIORef mempty
    sRef     <- newIORef mempty
    cRef     <- newIORef mempty
    hRef     <- newIORef mempty
    return (Space bRef sRef cRef hRef spacePtr)

  explSet _ _ _ = return ()
  explGet _ _ = return ()
  explDestroy _ _ = return ()
  explMembers _ = return mempty
  explExists _ _ = return False
  explGetUnsafe _ _ = return (error "Can't produce a Physics")
  explSetMaybe _ _ _ = return ()

-- Gravity
earthGravity :: Gravity
earthGravity = Gravity $ V2 0 (-9.81)

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
  explSet (Space _ _ _ _ spcPtr) _ (Gravity v) = setGravity spcPtr v
  explGet (Space _ _ _ _ spcPtr) _ = Gravity <$> getGravity spcPtr
  explSetMaybe  = explSet
  explGetUnsafe = explGet

-- Iterations
getIterations :: SpacePtr -> IO Int
getIterations spacePtr = withForeignPtr spacePtr $ \space -> fromIntegral <$> [C.exp| int { cpSpaceGetIterations ($(cpSpace* space)) } |]

setIterations :: SpacePtr -> Int -> IO ()
setIterations spacePtr (fromIntegral -> its) = withForeignPtr spacePtr $ \space -> [C.block| void {
  cpSpaceSetIterations($(cpSpace* space), $(int its));
  } |]

instance Component Iterations where
  type Storage Iterations = Space Iterations

instance Has w Physics => Has w Iterations where
  getStore = (cast :: Space Physics -> Space Iterations) <$> getStore

instance GlobalStore (Space Iterations)

instance Store (Space Iterations) where
  type Stores (Space Iterations) = Iterations
  type SafeRW (Space Iterations) = Iterations
  initStore = error "Initializing space from non-Physics store"
  explDestroy _ _ = return ()
  explMembers _   = return mempty
  explExists _ _  = return False
  explSet (Space _ _ _ _ spcPtr) _ (Iterations v) = setIterations spcPtr v
  explGet (Space _ _ _ _ spcPtr) _ = Iterations <$> getIterations spcPtr
  explSetMaybe  = explSet
  explGetUnsafe = explGet

