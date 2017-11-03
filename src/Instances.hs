{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
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
import qualified Data.Vector.Unboxed as U
import Apecs.Types
import Apecs.TH
import Data.Monoid ((<>))
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.IORef
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

import Context

C.context phycsCtx
C.include "<chipmunk.h>"

defaultSetMaybe s ety Nothing = explDestroy s ety
defaultSetMaybe s ety (Just x) = explSet s ety x

stepPhysics :: Has w Body => Double -> System w ()
stepPhysics (realToFrac -> dT) = do
  Space _ spacePtr :: Space Body <- getStore
  liftIO $ withForeignPtr spacePtr $ \space -> [C.exp| void { cpSpaceStep( $(cpSpace* space), $(double dT) ) } |]

destroyBody :: Ptr Body -> IO ()
destroyBody  bodyPtr  = [C.block| void { cpBodyDestroy ($(cpBody*  bodyPtr));  cpBodyFree ($(cpBody*  bodyPtr));  } |]
destroyShape shapePtr = [C.block| void { cpShapeDestroy($(cpShape* shapePtr)); cpShapeFree($(cpShape* shapePtr)); } |]

-- TODO: learn to hsc
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

getGravity :: ForeignPtr FrnSpace -> IO (V2 Double)
getGravity spacePtr = withForeignPtr spacePtr $ \space -> do
  x <- [C.exp| double { cpSpaceGetGravity ($(cpSpace* space)).x } |]
  y <- [C.exp| double { cpSpaceGetGravity ($(cpSpace* space)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setGravity :: ForeignPtr FrnSpace -> V2 Double -> IO ()
setGravity spacePtr (V2 (realToFrac -> x) (realToFrac -> y)) = withForeignPtr spacePtr $ \space -> [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpSpaceSetGravity($(cpSpace* space), vec);
  } |]

getMass :: Ptr Body -> IO Double
getMass bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMass ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMass :: Ptr Body -> Double -> IO ()
setMass bodyPtr (realToFrac -> mass) = [C.exp| void { cpBodySetMass($(cpBody* bodyPtr), $(double mass)); } |]

getMoment :: Ptr Body -> IO Double
getMoment bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMoment ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMoment :: Ptr Body -> Double -> IO ()
setMoment bodyPtr (realToFrac -> moment) = [C.exp| void { cpBodySetMoment($(cpBody* bodyPtr), $(double moment)); } |]

instance Component Body where
  type Storage Body = Space Body

instance Store (Space Body) where
  type Stores (Space Body) = Body
  type SafeRW (Space Body) = Maybe Body
  initStore = do
    spaceRaw     <- [C.exp| cpSpace* { cpSpaceNew() } |]
    spaceManaged <- newForeignPtr spaceRaw [C.exp| void { cpSpaceFree($(cpSpace* spaceRaw)) } |] -- FIXME: deallocate all map entries
    mapRef       <- newIORef mempty
    return (Space mapRef spaceManaged)

  explSet (Space mapRef spcPtr) ety btype = do
    rd <- M.lookup ety <$> readIORef mapRef
    bdyPtr <- case rd of
                Just (b, _) -> return b
                Nothing     -> withForeignPtr spcPtr $ \space -> do
                   bodyPtr <- [C.block| cpBody* { cpBody* body = cpBodyNew(0,0);
                                                  cpSpaceAddBody($(cpSpace* space), body);
                                                  return body; } |]
                   modifyIORef' mapRef (M.insert ety (bodyPtr, []))
                   return bodyPtr
    let bInt = fromIntegral $ fromEnum btype
    [C.exp| void { cpBodySetType($(cpBody* bdyPtr), $(int bInt)) } |]

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return Nothing
      Just (bdyPtr, _) -> withForeignPtr spcPtr $ \space -> do
        bInt <- [C.exp| int { cpBodyGetType($(cpBody* bdyPtr)) } |]
        return (Just . toEnum . fromIntegral $ bInt)

  explDestroy (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    modifyIORef' mapRef (M.delete ety)
    case rd of
      Just (b,_) -> destroyBody b
      _ -> return ()

  explMembers (Space mapRef spcPtr) = U.fromList . M.keys <$> readIORef mapRef

  explExists (Space mapRef spcPtr) ety = M.member ety <$> readIORef mapRef

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (bdyPtr, _) <- M.lookup ety <$> readIORef mapRef
    withForeignPtr spcPtr $ \space -> do
      bInt <- [C.exp| int { cpBodyGetType($(cpBody* bdyPtr)) } |]
      return (toEnum . fromIntegral $ bInt)

  explSetMaybe = defaultSetMaybe


instance Component Position where
  type Storage Position = Space Position

instance Has w Body => Has w Position where
  getStore = (cast :: Space Body -> Space Position) <$> getStore

instance Store (Space Position) where
  type Stores (Space Position) = Position
  type SafeRW (Space Position) = Maybe Position
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return Nothing
      Just (b, _) -> Just . Position <$> getPosition b

  explSet (Space mapRef spcPtr) ety (Position vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return ()
      Just (b,_) -> setPosition b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (bdyPtr, _) <- M.lookup ety <$> readIORef mapRef
    Position <$> getPosition bdyPtr


instance Component Gravity where
  type Storage Gravity = Space Gravity

instance Has w Body => Has w Gravity where
  getStore = (cast :: Space Body -> Space Gravity) <$> getStore

instance GlobalStore (Space Gravity)

instance Store (Space Gravity) where
  type Stores (Space Gravity) = Gravity
  type SafeRW (Space Gravity) = Gravity
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers _   = return mempty
  explExists _ _  = return False
  explSet (Space mapRef spcPtr) _ (Gravity v) = setGravity spcPtr v
  explGet (Space mapRef spcPtr) _ = Gravity <$> getGravity spcPtr
  explSetMaybe  = explSet
  explGetUnsafe = explGet


instance Component Mass where
  type Storage Mass = Space Mass

instance Has w Body => Has w Mass where
  getStore = (cast :: Space Body -> Space Mass) <$> getStore

instance Store (Space Mass) where
  type Stores (Space Mass) = Mass
  type SafeRW (Space Mass) = Maybe Mass
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return Nothing
      Just (b, _) -> Just . Mass <$> getMass b

  explSet (Space mapRef spcPtr) ety (Mass vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return ()
      Just (b,_) -> setMass b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (bdyPtr, _) <- M.lookup ety <$> readIORef mapRef
    Mass <$> getMass bdyPtr


instance Component Moment where
  type Storage Moment = Space Moment

instance Has w Body => Has w Moment where
  getStore = (cast :: Space Body -> Space Moment) <$> getStore

instance Store (Space Moment) where
  type Stores (Space Moment) = Moment
  type SafeRW (Space Moment) = Maybe Moment
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return Nothing
      Just (b, _) -> Just . Moment <$> getMoment b

  explSet (Space mapRef spcPtr) ety (Moment vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing -> return ()
      Just (b,_) -> setMoment b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (bdyPtr, _) <- M.lookup ety <$> readIORef mapRef
    Moment <$> getMoment bdyPtr

  

instance Cast (Space a) (Space b) where
  cast (Space c ref) = Space c ref

