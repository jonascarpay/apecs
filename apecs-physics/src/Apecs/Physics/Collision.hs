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

module Apecs.Physics.Collision
  ( defaultHandler
  , mkBeginCB, mkSeparateCB, mkPreSolveCB, mkPostSolveCB
  , addPostStepCallback
  ) where

import           Apecs
import           Apecs.Core
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Body  ()
import           Apecs.Physics.Space ()
import           Apecs.Physics.Types

C.context (phycsCtx `mappend` C.funCtx)
C.include "<chipmunk.h>"
C.include "<chipmunk_structs.h>"

defaultHandler :: CollisionHandler
defaultHandler = CollisionHandler (Wildcard 0) Nothing Nothing Nothing Nothing

mkCollision :: Ptr Collision -> IO Collision
mkCollision arb = do
  nx <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).x } |]
  ny <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).y } |]
  ba <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (ba->userData); } |]
  bb <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (bb->userData); } |]
  sa <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_SHAPES($(cpArbiter* arb), sa, sb); return (intptr_t) (sa->userData); } |]
  sb <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_SHAPES($(cpArbiter* arb), sa, sb); return (intptr_t) (sb->userData); } |]
  return $ Collision (V2 nx ny) (Entity ba) (Entity bb) (Entity sa) (Entity sb)

mkBeginCB :: MonadIO m => (Collision -> SystemT w IO Bool) -> SystemT w m BeginCB
mkBeginCB sys = do
    w <- ask

    let cb arb _ _ = do
          col <- liftIO $ mkCollision arb
          r <- runSystem (sys col) w
          return . fromIntegral . fromEnum $ r

    return (BeginCB cb)

mkSeparateCB :: MonadIO m => (Collision -> SystemT w IO ()) -> SystemT w m SeparateCB
mkSeparateCB sys = do
    w <- ask

    let cb arb _ _ = do
          col <- liftIO $ mkCollision arb
          runSystem (sys col) w

    return (SeparateCB cb)


mkPreSolveCB :: MonadIO m => (Collision -> SystemT w IO Bool) -> SystemT w m PreSolveCB
mkPreSolveCB sys = (\(BeginCB cb) -> PreSolveCB cb) <$> mkBeginCB sys

mkPostSolveCB :: MonadIO m => (Collision -> SystemT w IO ()) -> SystemT w m PostSolveCB
mkPostSolveCB sys = (\(SeparateCB cb) -> PostSolveCB cb) <$> mkSeparateCB sys

newCollisionHandler :: SpacePtr -> CollisionHandler -> Int -> IO (Ptr CollisionHandler)
newCollisionHandler spcPtr (CollisionHandler source begin separate presolve postsolve) (fromIntegral -> ety) =
  withForeignPtr spcPtr $ \space -> do
    handler <- case source of
      Between (CollisionType cta) (CollisionType ctb)
        -> [C.exp| cpCollisionHandler* {cpSpaceAddCollisionHandler($(cpSpace* space), $(uintptr_t cta), $(uintptr_t ctb))}|]
      Wildcard (CollisionType ct)
        -> [C.exp| cpCollisionHandler* {cpSpaceAddWildcardHandler($(cpSpace* space), $(uintptr_t ct))}|]


    [C.exp| void { $(cpCollisionHandler* handler)->userData = (void*) $(intptr_t ety) }|]

    forM_ begin$ \(BeginCB cb) -> do
      funPtr <- liftIO$ $(C.mkFunPtr [t| BeginFunc |]) cb
      let fn = castFunPtrToPtr funPtr
      [C.exp| void { $(cpCollisionHandler* handler)->beginFunc = $(void* fn) }|]

    forM_ separate$ \(SeparateCB cb) -> do
      funPtr <- liftIO$ $(C.mkFunPtr [t| SeparateFunc |]) cb
      let fn = castFunPtrToPtr funPtr
      [C.exp| void { $(cpCollisionHandler* handler)->separateFunc = $(void* fn) }|]

    forM_ presolve$ \(PreSolveCB cb) -> do
      funPtr <- liftIO$ $(C.mkFunPtr [t| PreSolveFunc |]) cb
      let fn = castFunPtrToPtr funPtr
      [C.exp| void { $(cpCollisionHandler* handler)->preSolveFunc = $(void* fn) }|]

    forM_ postsolve$ \(PostSolveCB cb) -> do
      funPtr <- liftIO$ $(C.mkFunPtr [t| PostSolveFunc |]) cb
      let fn = castFunPtrToPtr funPtr
      [C.exp| void { $(cpCollisionHandler* handler)->postSolveFunc = $(void* fn) }|]

    return handler

destroyCollisionHandler :: Ptr CollisionHandler -> IO ()
destroyCollisionHandler = error "Destroy CollisionHandler not yet implemented"

instance Component CollisionHandler where
  type Storage CollisionHandler = Space CollisionHandler

instance (MonadIO m, Has w m Physics) => Has w m CollisionHandler where
  getStore = (cast :: Space Physics -> Space CollisionHandler) <$> getStore

instance (MonadIO m) => ExplSet m (Space CollisionHandler) where
  explSet sp@(Space _ _ _ hMap spcPtr) ety handler = liftIO $ do
    explDestroy sp ety
    hPtr <- newCollisionHandler spcPtr handler ety
    modifyIORef' hMap (M.insert ety (Record hPtr handler))

instance (MonadIO m) => ExplDestroy m (Space CollisionHandler) where
  explDestroy (Space _ _ _ hMap _) ety = liftIO $ do
    rd <- M.lookup ety <$> readIORef hMap
    forM_ rd$ \(Record c _) -> destroyCollisionHandler c >> modifyIORef' hMap (M.delete ety)

instance (MonadIO m) => ExplMembers m (Space CollisionHandler) where
  explMembers (Space _ _ _ hMap _) = liftIO $ U.fromList . M.keys <$> readIORef hMap

instance (MonadIO m) => ExplGet m (Space CollisionHandler) where
  explExists (Space _ _ _ hMap _) ety = liftIO $ M.member ety <$> readIORef hMap
  explGet (Space _ _ _ hMap _) ety = liftIO $ do
    Just (Record _ handler) <- M.lookup ety <$> readIORef hMap
    return handler

-- | Add an action that will be executed after the physics engine is done processing the current step. Since you generally cannot modify the physics space while the engine is handling collisions, 'addPostStepCallback' is the primary way of making changes to the physics space with a 'CollisionHandler' in a safe manner.
-- Please note that you should only use this function for callbacks in conjunction with a 'CollisionHandler'!
addPostStepCallback :: (Has w m Physics, MonadIO m) => Int -> SystemT w IO () -> SystemT w m ()
addPostStepCallback (toEnum -> k) systemCallback= do
  w <- ask
  let callback _ _ _ = runSystem systemCallback w
  (Space _ _ _ _ spcPtr) :: Space Physics <- getStore
  liftIO $ withForeignPtr spcPtr $ \space -> do
    funPtr <- liftIO$ $(C.mkFunPtr [t| Ptr FrnSpace -> Ptr () -> Ptr () -> IO () |]) callback
    let fn = castFunPtrToPtr funPtr
    [C.block| void {
      int *data = 0;
      cpSpaceAddPostStepCallback($(cpSpace *space), $(void*fn),$(int k), &data);
    } |]
  pure ()
