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
import Wrapper

C.context phycsCtx
C.include "<chipmunk.h>"

stepPhysicsSys dT = do
  Space _ spacePtr :: Space Body <- getStore
  liftIO$ stepPhysics spacePtr dT

defaultSetMaybe s ety Nothing = explDestroy s ety
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
                Just (b, _) -> return b
                Nothing -> do
                  bodyPtr <- newBody spcPtr
                  modifyIORef' mapRef (M.insert ety (bodyPtr, []))
                  return bodyPtr
    setBodyType bdyPtr btype

  explGet (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    case rd of Nothing -> return Nothing
               Just (bdyPtr, _) -> Just <$> getBodyType bdyPtr

  explDestroy (Space mapRef spcPtr) ety = do
    rd <- M.lookup ety <$> readIORef mapRef
    modifyIORef' mapRef (M.delete ety)
    case rd of Just (b,_) -> destroyBody b
               _ -> return ()

  explMembers (Space mapRef spcPtr) = U.fromList . M.keys <$> readIORef mapRef

  explExists (Space mapRef spcPtr) ety = M.member ety <$> readIORef mapRef

  explGetUnsafe (Space mapRef spcPtr) ety = do
    Just (bdyPtr, _) <- M.lookup ety <$> readIORef mapRef
    getBodyType bdyPtr

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


instance Component Shape where
  type Storage Shape = Space Shape

instance Has w Body => Has w Shape where
  getStore = (cast :: Space Body -> Space Shape) <$> getStore

instance Store (Space Shape) where
  type Stores (Space Shape) = Shape
  type SafeRW (Space Shape) = Maybe Shape
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  explMembers s = explMembers (cast s :: Space Body)
  explExists s ety = explExists (cast s :: Space Body) ety
  explSetMaybe = defaultSetMaybe

  -- TODO: get/set shapes

instance Cast (Space a) (Space b) where
  cast (Space c ref) = Space c ref

