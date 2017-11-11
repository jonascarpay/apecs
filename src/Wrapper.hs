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

module Wrapper where

import           Apecs
import           Apecs.TH
import           Apecs.Types
import           Control.Monad
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

-- Body
newBody :: SpacePtr -> IO (Ptr Body)
newBody spacePtr = withForeignPtr spacePtr $ \space -> [C.block| cpBody* {
    cpBody* body = cpBodyNew(0,0);
    cpSpaceAddBody($(cpSpace* space), body);
    return body; } |]

setBodyType :: Ptr Body -> Body -> IO ()
setBodyType bodyPtr (fromIntegral . fromEnum -> bodyInt) =
  [C.exp| void { cpBodySetType($(cpBody* bodyPtr), $(int bodyInt)) } |]

getBodyType :: Ptr Body -> IO Body
getBodyType bodyPtr = toEnum . fromIntegral <$> [C.exp| int { cpBodyGetType($(cpBody* bodyPtr)) } |]

destroyBody :: Ptr Body -> IO ()
destroyBody bodyPtr = [C.block| void {
  cpBodyDestroy ($(cpBody* bodyPtr));
  cpBodyFree    ($(cpBody* bodyPtr)); }|]

-- Position
getPosition :: Ptr Body -> IO (V2 Double)
getPosition bodyPtr = do
  x <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setPosition :: Ptr Body -> V2 Double -> IO ()
setPosition bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpBodySetPosition($(cpBody* bodyPtr), vec);
  } |]

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

-- Mass
getMass :: Ptr Body -> IO Double
getMass bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMass ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMass :: Ptr Body -> Double -> IO ()
setMass bodyPtr (realToFrac -> mass) = [C.exp| void { cpBodySetMass($(cpBody* bodyPtr), $(double mass)); } |]

-- Moment
getMoment :: Ptr Body -> IO Double
getMoment bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMoment ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMoment :: Ptr Body -> Double -> IO ()
setMoment bodyPtr (realToFrac -> moment) = [C.exp| void { cpBodySetMoment($(cpBody* bodyPtr), $(double moment)); } |]

