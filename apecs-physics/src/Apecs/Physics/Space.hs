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
import           Apecs.Core
import           Control.Monad.IO.Class (liftIO, MonadIO)
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

stepPhysics :: MonadIO m => Has w m Physics => Double -> SystemT w m ()
stepPhysics dT = do
  s :: Space Physics <- getStore
  liftIO$ explStepPhysics (spacePtr s) dT

instance Component Physics where
  type Storage Physics = Space Physics

type instance Elem (Space Physics) = Physics

instance MonadIO m => ExplInit m (Space Physics) where
  explInit = liftIO $ do
    spacePtr <- newSpace
    bRef     <- newIORef mempty
    sRef     <- newIORef mempty
    cRef     <- newIORef mempty
    hRef     <- newIORef mempty
    return (Space bRef sRef cRef hRef spacePtr)

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

instance (MonadIO m, Has w m Physics) => Has w m Gravity where
  getStore = (cast :: Space Physics -> Space Gravity) <$> getStore

type instance Elem (Space Gravity) = Gravity

instance MonadIO m => ExplGet m (Space Gravity) where
  explExists _ _  = return True
  explGet (Space _ _ _ _ spcPtr) _ = liftIO $ Gravity <$> getGravity spcPtr
instance MonadIO m => ExplSet m (Space Gravity) where
  explSet (Space _ _ _ _ spcPtr) _ (Gravity v) = liftIO $ setGravity spcPtr v

-- Iterations
getIterations :: SpacePtr -> IO Int
getIterations spacePtr = withForeignPtr spacePtr $ \space -> fromIntegral <$> [C.exp| int { cpSpaceGetIterations ($(cpSpace* space)) } |]

setIterations :: SpacePtr -> Int -> IO ()
setIterations spacePtr (fromIntegral -> its) = withForeignPtr spacePtr $ \space -> [C.block| void {
  cpSpaceSetIterations($(cpSpace* space), $(int its));
  } |]

instance Component Iterations where
  type Storage Iterations = Space Iterations

instance (MonadIO m, Has w m Physics) => Has w m Iterations where
  getStore = (cast :: Space Physics -> Space Iterations) <$> getStore

type instance Elem (Space Iterations) = Iterations

instance MonadIO m => ExplGet m (Space Iterations) where
  explExists _ _  = return False
  explGet (Space _ _ _ _ spcPtr) _ = liftIO $ Iterations <$> getIterations spcPtr
instance MonadIO m => ExplSet m (Space Iterations) where
  explSet (Space _ _ _ _ spcPtr) _ (Iterations v) = liftIO $ setIterations spcPtr v
