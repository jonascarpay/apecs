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

module Body where

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

import           Instances
import           Types

C.context phycsCtx
C.include "<chipmunk.h>"

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
                  bodyPtr <- newBody spcPtr
                  modifyIORef' mapRef (M.insert ety (BodyRecord bodyPtr (Shapes []) []))
                  return bodyPtr
    setBodyType bdyPtr btype

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of Nothing                 -> return Nothing
               Just (BodyRecord b _ _) -> Just <$> getBodyType b

  explDestroy (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    modifyIORef' mapRef (M.delete ety)
    case rd of Just (BodyRecord b _ _ ) -> destroyBody b
               _                        -> return ()

  explMembers (Space mapRef spcPtr) = U.fromList . M.keys <$> readIORef mapRef

  explExists (Space mapRef spcPtr) ety = M.member ety <$> readIORef mapRef

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
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
  cpBodySetPosition($(cpBody* bodyPtr), vec);
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

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return Nothing
      Just (BodyRecord b _ _ ) -> Just . Position <$> getPosition b

  explSet (Space mapRef spcPtr) ety (Position vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return ()
      Just (BodyRecord b _ _ ) -> setPosition b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Position <$> getPosition b

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

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return Nothing
      Just (BodyRecord b _ _ ) -> Just . Mass <$> getMass b

  explSet (Space mapRef spcPtr) ety (Mass vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                  -> return ()
      Just (BodyRecord b _ _ ) -> setMass b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
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

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return Nothing
      Just (BodyRecord b _ _) -> Just . Moment <$> getMoment b

  explSet (Space mapRef spcPtr) ety (Moment vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return ()
      Just (BodyRecord b _ _) -> setMoment b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Moment <$> getMoment b

-- Angle
getAngle :: Ptr Body -> IO Double
getAngle bodyPtr = do
  angle <- [C.exp| double { cpBodyGetAngle ($(cpBody* bodyPtr)) } |]
  return (realToFrac angle)

setAngle :: Ptr Body -> Double -> IO ()
setAngle bodyPtr (realToFrac -> angle) = [C.exp| void { cpBodySetAngle($(cpBody* bodyPtr), $(double angle)); } |]

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

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return Nothing
      Just (BodyRecord b _ _) -> Just . Angle <$> getAngle b

  explSet (Space mapRef spcPtr) ety (Angle vec) = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of
      Nothing                 -> return ()
      Just (BodyRecord b _ _) -> setAngle b vec

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (BodyRecord b _ _) <- M.lookup ety <$> readIORef mapRef
    Angle <$> getAngle b
