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

-- | Necessary to pass the @w@ argument, but I'm interested in a way to remove this
makeCallback :: (CollisionPair -> System w a) -> System w (Callback a)
makeCallback sys = do
    w <- System ask
    return . Callback $ \pair -> runSystem (sys pair) w

newCollisionHandler :: SpacePtr -> CollisionHandler -> Int -> IO (Ptr CollisionHandler)
newCollisionHandler spcPtr (CollisionHandler cta ctb begin separate) (fromIntegral -> ety) =
  withForeignPtr spcPtr $ \space -> do
    handler <- [C.exp| cpCollisionHandler* {cpSpaceAddCollisionHandler($(cpSpace* space), $(unsigned int cta), $(unsigned int ctb))}|]

    forM_ begin $ \(Callback cb) ->
      let sys_ arb spc _ = do nx <- realToFrac <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).x } |]
                              ny <- realToFrac <$> [C.exp| double { cpArbiterGetNormal($(cpArbiter* arb)).y } |]
                              ea <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (ba->userData); } |]
                              eb <- fromIntegral <$> [C.block| unsigned int { CP_ARBITER_GET_BODIES($(cpArbiter* arb), ba, bb); return (intptr_t) (bb->userData); } |]
                              fromIntegral . fromEnum <$> cb (CollisionPair (V2 nx ny) (Entity ea) (Entity eb))

        -- FIXME:
        -- It seems like this callback is never called.
        -- The default beginFunc is properly called if we don't change this value, and we get an error on collision if we set it to NULL,
        -- so it _is_ used at runtime...
       in [C.block| void { $(cpCollisionHandler* handler)->beginFunc = $fun:(unsigned char (*sys_)(cpArbiter*,cpSpace*,cpDataPointer));
                           $(cpCollisionHandler* handler)->userData = (void*) $(intptr_t ety);
                         }|]

    forM_ separate $ \e -> putStrLn "Separation callbacks not yet implemented"

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
