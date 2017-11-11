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
  Space _ spacePtr :: Space Body <- getStore
  liftIO$ stepPhysics spacePtr dT

defaultSetMaybe s ety Nothing  = explDestroy s ety
defaultSetMaybe s ety (Just x) = explSet s ety x

instance Component Body where
  type Storage Body = Space Body

instance Store (Space Body) where
  type Stores (Space Body) = Body
  type SafeRW (Space Body) = Maybe Body
  initStore = do
    spacePtr <- newSpace
    mapRef   <- newIORef mempty
    return (Space mapRef spacePtr)

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


