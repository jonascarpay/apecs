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

module Apecs.Physics.Collision where

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

import           Apecs.Physics.Body  ()
import           Apecs.Physics.Space ()
import           Apecs.Physics.Types

C.context (phycsCtx `mappend` C.funCtx)
C.include "<chipmunk.h>"
C.include "<chipmunk_structs.h>"

defaultHandler :: CollisionHandler
defaultHandler = CollisionHandler (Wildcard 0) Nothing Nothing Nothing Nothing

mkBeginCB :: (Collision -> System w Bool) -> System w BeginCB
mkBeginCB sys = do
    w <- System ask

    let cb arb _ _ = do
          nx <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).x } |]
          ny <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).y } |]
          ea <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (ba->userData); } |]
          eb <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (bb->userData); } |]
          r <- liftIO$ runSystem (sys (Collision (V2 nx ny) (Entity ea) (Entity eb))) w
          return . fromIntegral . fromEnum $ r

    return (BeginCB cb)

mkSeparateCB :: (Collision -> System w ()) -> System w SeparateCB
mkSeparateCB sys = do
    w <- System ask

    let cb arb _ _ = do
          nx <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).x } |]
          ny <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).y } |]
          ea <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (ba->userData); } |]
          eb <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (bb->userData); } |]
          liftIO$ runSystem (sys (Collision (V2 nx ny) (Entity ea) (Entity eb))) w

    return (SeparateCB cb)


mkPreSolveCB :: (Collision -> System w Bool) -> System w PreSolveCB
mkPreSolveCB sys = (\(BeginCB cb) -> PreSolveCB cb) <$> mkBeginCB sys

mkPostSolveCB :: (Collision -> System w ()) -> System w PostSolveCB
mkPostSolveCB sys = (\(SeparateCB cb) -> PostSolveCB cb) <$> mkSeparateCB sys

newCollisionHandler :: SpacePtr -> CollisionHandler -> Int -> IO (Ptr CollisionHandler)
newCollisionHandler spcPtr (CollisionHandler source begin separate presolve postsolve) (fromIntegral -> ety) =
  withForeignPtr spcPtr $ \space -> do
    handler <- case source of
                 Between cta ctb -> [C.exp| cpCollisionHandler* {cpSpaceAddCollisionHandler($(cpSpace* space), $(unsigned int cta), $(unsigned int ctb))}|]
                 Wildcard ct     -> [C.exp| cpCollisionHandler* {cpSpaceAddWildcardHandler($(cpSpace* space), $(unsigned int ct))}|]

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

instance Has w Physics => Has w CollisionHandler where
  getStore = (cast :: Space Physics -> Space CollisionHandler) <$> getStore

instance Store (Space CollisionHandler) where
  type Stores (Space CollisionHandler) = CollisionHandler
  type SafeRW (Space CollisionHandler) = Maybe CollisionHandler
  initStore = error "Initializing space from non-Physics store"

  explSet sp@(Space _ _ _ hMap spcPtr) ety handler = do
    explDestroy sp ety
    hPtr <- newCollisionHandler spcPtr handler ety
    modifyIORef' hMap (M.insert ety hPtr)

  explDestroy (Space _ _ _ hMap _) ety = do
    rd <- M.lookup ety <$> readIORef hMap
    forM_ rd$ \c -> destroyCollisionHandler c >> modifyIORef' hMap (M.delete ety)

  explMembers (Space _ _ _ hMap _) = U.fromList . M.keys <$> readIORef hMap
  explExists (Space _ _ _ hMap _) ety = M.member ety <$> readIORef hMap
  explSetMaybe = defaultSetMaybe

  explGet s ety = do
    e <- explExists s ety
    if e then Just <$> explGetUnsafe s ety else return Nothing
  explGetUnsafe _ _ = return (error "CollisionHandler is a read-only component")

