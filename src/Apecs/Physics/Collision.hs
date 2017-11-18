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
import           Apecs.Types
import           Control.Monad
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2

import           Apecs.Physics.Body
import           Apecs.Physics.Space
import           Apecs.Physics.Types

C.context (phycsCtx `mappend` C.funCtx)
C.include "<chipmunk.h>"
C.include "<chipmunk_structs.h>"

makeCallback :: (CollisionPair -> System w Bool) -> System w BeginCB
makeCallback sys = do
    w <- System ask

    let cb arb _ _ = do
          nx <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).x } |]
          ny <- realToFrac   <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).y } |]
          ea <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (ba->userData); } |]
          eb <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (bb->userData); } |]
          r <- liftIO$ runSystem (sys (CollisionPair (V2 nx ny) (Entity ea) (Entity eb))) w
          return . fromIntegral . fromEnum $ r

    a <- liftIO$ $(C.mkFunPtr [t| Ptr CollisionPair -> Ptr FrnSpace -> C.CUInt -> IO C.CUChar |]) cb
    return (BeginCB a)

newCollisionHandler :: SpacePtr -> CollisionHandler -> Int -> IO (Ptr CollisionHandler)
newCollisionHandler spcPtr (CollisionHandler cta ctb begin separate) (fromIntegral -> ety) =
  withForeignPtr spcPtr $ \space -> do
    handler <- [C.exp| cpCollisionHandler* {cpSpaceAddCollisionHandler($(cpSpace* space), $(unsigned int cta), $(unsigned int ctb))}|]
    forM_ begin$ \(BeginCB cb) -> do
      let fn = $(C.peekFunPtr [t| BeginFunc |]) cb
      [C.block| void {
      $(cpCollisionHandler* handler)->beginFunc = $fun:(unsigned char (*fn)(cpArbiter*, cpSpace*, cpDataPointer));
      $(cpCollisionHandler* handler)->userData = (void*) $(intptr_t ety); }|]

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

  explSet sp@(Space bMap cMap hMap spcPtr) ety handler = do
    explDestroy sp ety
    hPtr <- newCollisionHandler spcPtr handler ety
    modifyIORef' hMap (M.insert ety hPtr)

  explDestroy (Space _ _ hMap _) ety = do
    rd <- M.lookup ety <$> readIORef hMap
    modifyIORef' hMap (M.delete ety)
    case rd of Just c -> destroyCollisionHandler c
               _      -> return ()

  explMembers (Space _ cMap _ _) = U.fromList . M.keys <$> readIORef cMap

  explExists (Space _ cMap _ _) ety = M.member ety <$> readIORef cMap

  explSetMaybe = defaultSetMaybe
