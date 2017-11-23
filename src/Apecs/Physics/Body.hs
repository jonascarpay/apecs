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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Apecs.Physics.Body where

import           Apecs
import           Apecs.Stores        (defaultSetMaybe)
import           Apecs.Types
import           Control.Monad
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Space ()
import           Apecs.Physics.Types

C.context phycsCtx
C.include "<chipmunk.h>"

-- Body
newBody :: SpacePtr -> Int -> IO (Ptr Body)
newBody spacePtr (fromIntegral -> ety) = withForeignPtr spacePtr $ \space -> [C.block| cpBody* {
    cpBody* body = cpBodyNew(0,0);
    cpSpaceAddBody($(cpSpace* space), body);
    cpBodySetUserData(body, (void*) $(intptr_t ety));
    return body; } |]

setBodyType :: Ptr Body -> Body -> IO ()
setBodyType bodyPtr (fromIntegral . fromEnum -> bodyInt) =
  [C.exp| void { cpBodySetType($(cpBody* bodyPtr), $(int bodyInt)) } |]

getBodyType :: Ptr Body -> IO Body
getBodyType bodyPtr = toEnum . fromIntegral <$> [C.exp| int { cpBodyGetType($(cpBody* bodyPtr)) } |]

destroyBody :: SpacePtr -> Ptr Body -> IO ()
destroyBody spacePtr bodyPtr = withForeignPtr spacePtr $ \space -> [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpSpaceRemoveBody($(cpSpace* space), body);
  cpBodyFree(body); }|]

instance Component Body where
  type Storage Body = Space Body

instance Has w Physics => Has w Body where
  getStore = (cast :: Space Physics -> Space Body) <$> getStore

instance Store (Space Body) where
  type Stores (Space Body) = Body
  type SafeRW (Space Body) = Maybe Body
  initStore = error "Initializing space from non-Physics store"

  explSet (Space bMap _ _ _ spcPtr) ety btype = do
    rd <- M.lookup ety <$> readIORef bMap
    bdyPtr <- case rd of
                Just bdyPtr -> return bdyPtr
                Nothing -> do
                  bdyPtr <- newBody spcPtr ety
                  modifyIORef' bMap (M.insert ety bdyPtr)
                  return bdyPtr
    setBodyType bdyPtr btype

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of Nothing -> return Nothing
               Just b  -> Just <$> getBodyType b

  explDestroy (Space bMap _ _ _ spc) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    modifyIORef' bMap (M.delete ety)
    case rd of Just b -> destroyBody spc b
               _      -> return ()

  explMembers (Space bMap _ _ _ _) = U.fromList . M.keys <$> readIORef bMap

  explExists (Space bMap _ _ _ _) ety = M.member ety <$> readIORef bMap

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just b <- M.lookup ety <$> readIORef bMap
    getBodyType b

  explSetMaybe = defaultSetMaybe

-- Position
getPosition :: Ptr Body -> IO (V2 Double)
getPosition bodyPtr = do
  x <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setPosition :: Ptr Body -> V2 Double -> IO ()
setPosition bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetPosition(body, vec);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(body->space, body);
  } |]

instance Component Position where
  type Storage Position = Space Position

instance Has w Physics => Has w Position where
  getStore = (cast :: Space Physics -> Space Position) <$> getStore

instance Store (Space Position) where
  type Stores (Space Position) = Position
  type SafeRW (Space Position) = Maybe Position
  initStore = error "Attempted to initialize a space from an Position component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just b  -> Just . Position <$> getPosition b

  explSet (Space bMap _ _ _ _) ety (Position vec) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd$ \b -> setPosition b vec

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just b <- M.lookup ety <$> readIORef bMap
    Position <$> getPosition b

-- Angle
getAngle :: Ptr Body -> IO Double
getAngle bodyPtr = do
  angle <- [C.exp| double { cpBodyGetAngle ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setAngle :: Ptr Body -> Double -> IO ()
setAngle bodyPtr (realToFrac -> angle) = [C.block| void {
  cpBody *body = $(cpBody* bodyPtr);
  cpBodySetAngle(body, $(double angle));
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(body->space, body);
  } |]
  -- FIXME reindex

instance Component Angle where
  type Storage Angle = Space Angle

instance Has w Physics => Has w Angle where
  getStore = (cast :: Space Physics -> Space Angle) <$> getStore

instance Store (Space Angle) where
  type Stores (Space Angle) = Angle
  type SafeRW (Space Angle) = Maybe Angle
  initStore = error "Attempted to initialize a space from an Angle component, use Physics instead"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space bMap _ _ _ _) ety = do
    rd <- M.lookup ety <$> readIORef bMap
    case rd of
      Nothing -> return Nothing
      Just b  -> Just . Angle <$> getAngle b

  explSet (Space bMap _ _ _ _) ety (Angle vec) = do
    rd <- M.lookup ety <$> readIORef bMap
    forM_ rd $ \b -> setAngle b vec

  explGetUnsafe (Space bMap _ _ _ _) ety = do
    Just b <- M.lookup ety <$> readIORef bMap
    Angle <$> getAngle b
