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
import           Apecs.Types
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Space
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

destroyBody :: Ptr Body -> IO ()
destroyBody bodyPtr = [C.block| void {
  cpBodyDestroy ($(cpBody* bodyPtr));
  cpBodyFree    ($(cpBody* bodyPtr)); }|]

instance Component Body where
  type Storage Body = Space Body

instance Has w Physics => Has w Body where
  getStore = (cast :: Space Physics -> Space Body) <$> getStore

instance Store (Space Body) where
  type Stores (Space Body) = Body
  type SafeRW (Space Body) = Maybe Body
  initStore = error "Initializing space from non-Physics store"

  explSet (Space mapRef spcPtr) ety btype = do
    rd <- M.lookup ety <$> readIORef mapRef
    bdyPtr <- case rd of
                Just (BodyRecord b _ _) -> return b
                Nothing -> do
                  bodyPtr <- newBody spcPtr ety
                  modifyIORef' mapRef (M.insert ety (BodyRecord bodyPtr mempty []))
                  return bodyPtr
    setBodyType bdyPtr btype

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of Nothing                 -> return Nothing
               Just (BodyRecord b _ _) -> Just <$> getBodyType b

  explDestroy (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    modifyIORef' mapRef (M.delete ety)
    case rd of Just (BodyRecord b _ _ ) -> destroyBody b
               _                        -> return ()

  explMembers (Space mapRef _) = U.fromList . M.keys <$> readIORef mapRef

  explExists (Space mapRef _) ety = M.member ety <$> readIORef mapRef

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    getBodyType b

  explSetMaybe = defaultSetMaybe

-- Position
getPosition :: Ptr Body -> IO (V2 Double)
getPosition bodyPtr = do
  x <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).x } |]
  y <- [C.exp| double { cpBodyGetPosition ($(cpBody* bodyPtr)).y } |]
  return (V2 (realToFrac x) (realToFrac y))

setPosition :: SpacePtr -> Ptr Body -> V2 Double -> IO ()
setPosition spcPtr bodyPtr (V2 (realToFrac -> x) (realToFrac -> y)) = withForeignPtr spcPtr $ \space -> [C.block| void {
  const cpVect vec = { $(double x), $(double y) };
  cpBodySetPosition($(cpBody* bodyPtr), vec);
  cpSpaceReindexShapesForBody($(cpSpace* space), $(cpBody* bodyPtr));
  } |]

instance Component Position where
  type Storage Position = Space Position

instance Has w Physics => Has w Position where
  getStore = (cast :: Space Physics -> Space Position) <$> getStore

instance Store (Space Position) where
  type Stores (Space Position) = Position
  type SafeRW (Space Position) = Maybe Position
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return Nothing
      Just (BodyRecord b _ _ ) -> Just . Position <$> getPosition b

  explSet (Space mapRef spcPtr) ety (Position vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return ()
      Just (BodyRecord b _ _ ) -> setPosition spcPtr b vec

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Position <$> getPosition b

-- Angle
getAngle :: Ptr Body -> IO Double
getAngle bodyPtr = do
  angle <- [C.exp| double { cpBodyGetAngle ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setAngle :: SpacePtr -> Ptr Body -> Double -> IO ()
setAngle spcPtr bodyPtr (realToFrac -> angle) = withForeignPtr spcPtr $ \space -> [C.block| void {
  cpBodySetAngle($(cpBody* bodyPtr), $(double angle));
  cpSpaceReindexShapesForBody($(cpSpace* space), $(cpBody* bodyPtr));
  } |]

instance Component Angle where
  type Storage Angle = Space Angle

instance Has w Physics => Has w Angle where
  getStore = (cast :: Space Physics -> Space Angle) <$> getStore

instance Store (Space Angle) where
  type Stores (Space Angle) = Angle
  type SafeRW (Space Angle) = Maybe Angle
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return Nothing
      Just (BodyRecord b _ _) -> Just . Angle <$> getAngle b

  explSet (Space mapRef spcPtr) ety (Angle vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return ()
      Just (BodyRecord b _ _) -> setAngle spcPtr b vec

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Angle <$> getAngle b

-- Mass
getMass :: Ptr Body -> IO Double
getMass bodyPtr = do
  mass <- [C.exp| double { cpBodyGetMass ($(cpBody* bodyPtr)) } |]
  return (realToFrac mass)

setMass :: Ptr Body -> Double -> IO ()
setMass bodyPtr (realToFrac -> mass) = [C.exp| void { cpBodySetMass($(cpBody* bodyPtr), $(double mass)); } |]

instance Component Mass where
  type Storage Mass = Space Mass

instance Has w Physics => Has w Mass where
  getStore = (cast :: Space Physics -> Space Mass) <$> getStore

instance Store (Space Mass) where
  type Stores (Space Mass) = Mass
  type SafeRW (Space Mass) = Maybe Mass
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return Nothing
      Just (BodyRecord b _ _ ) -> Just . Mass <$> getMass b

  explSet (Space mapRef _) ety (Mass vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return ()
      Just (BodyRecord b _ _ ) -> setMass b vec

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Mass <$> getMass b

-- Moment
getMoment :: Ptr Body -> IO Double
getMoment bodyPtr = do
  moment <- [C.exp| double { cpBodyGetMoment ($(cpBody* bodyPtr)) } |]
  return (realToFrac moment)

setMoment :: Ptr Body -> Double -> IO ()
setMoment bodyPtr (realToFrac -> moment) = [C.exp| void { cpBodySetMoment($(cpBody* bodyPtr), $(double moment)); } |]

instance Component Moment where
  type Storage Moment = Space Moment

instance Has w Physics => Has w Moment where
  getStore = (cast :: Space Physics -> Space Moment) <$> getStore

instance Store (Space Moment) where
  type Stores (Space Moment) = Moment
  type SafeRW (Space Moment) = Maybe Moment
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return Nothing
      Just (BodyRecord b _ _) -> Just . Moment <$> getMoment b

  explSet (Space mapRef _) ety (Moment vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return ()
      Just (BodyRecord b _ _) -> setMoment b vec

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Moment <$> getMoment b

-- Velocity
getVelocity :: Ptr Body -> IO Vec
getVelocity bodyPtr = do
  vx <- [C.exp| double { cpBodyGetVelocity ($(cpBody* bodyPtr)).x } |]
  vy <- [C.exp| double { cpBodyGetVelocity ($(cpBody* bodyPtr)).y } |]
  return $ V2 (realToFrac vx) (realToFrac vy)

setVelocity :: Ptr Body -> Vec -> IO ()
setVelocity bodyPtr (fmap realToFrac -> V2 x y) = [C.exp| void { cpBodySetVelocity($(cpBody* bodyPtr), cpv($(double x), $(double y))); } |]

instance Component Velocity where
  type Storage Velocity = Space Velocity

instance Has w Physics => Has w Velocity where
  getStore = (cast :: Space Physics -> Space Velocity) <$> getStore

instance Store (Space Velocity) where
  type Stores (Space Velocity) = Velocity
  type SafeRW (Space Velocity) = Maybe Velocity
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return Nothing
      Just (BodyRecord b _ _) -> Just . Velocity <$> getVelocity b

  explSet (Space mapRef _) ety (Velocity vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return ()
      Just (BodyRecord b _ _) -> setVelocity b vec

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Velocity <$> getVelocity b

-- AngularVelocity
getAngularVelocity :: Ptr Body -> IO Double
getAngularVelocity bodyPtr = do
  angular <- [C.exp| double { cpBodyGetAngularVelocity ($(cpBody* bodyPtr)) } |]
  return (realToFrac angular)

setAngularVelocity :: Ptr Body -> Double -> IO ()
setAngularVelocity bodyPtr (realToFrac -> angular) = [C.exp| void { cpBodySetAngularVelocity($(cpBody* bodyPtr), $(double angular)); } |]

instance Component AngularVelocity where
  type Storage AngularVelocity = Space AngularVelocity

instance Has w Physics => Has w AngularVelocity where
  getStore = (cast :: Space Physics -> Space AngularVelocity) <$> getStore

instance Store (Space AngularVelocity) where
  type Stores (Space AngularVelocity) = AngularVelocity
  type SafeRW (Space AngularVelocity) = Maybe AngularVelocity
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  explGet (Space mapRef _) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return Nothing
      Just (BodyRecord b _ _) -> Just . AngularVelocity <$> getAngularVelocity b

  explSet (Space mapRef _) ety (AngularVelocity vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return ()
      Just (BodyRecord b _ _) -> setAngularVelocity b vec

  explGetUnsafe (Space mapRef _) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    AngularVelocity <$> getAngularVelocity b

