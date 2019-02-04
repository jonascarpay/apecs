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
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ViewPatterns               #-}

module Apecs.Physics.Collision where

import           Apecs
import           Apecs.Core
import           Control.Monad
import qualified Data.IntMap         as M
import           Data.IORef
import qualified Data.Vector.Unboxed as U
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Ptr
import qualified Language.C.Inline   as C
import           Linear.V2
import           Data.Functor (($>))

import           Apecs.Physics.Body  ()
import           Apecs.Physics.Space ()
import           Apecs.Physics.Types

C.context (phycsCtx `mappend` C.funCtx)
C.include "<chipmunk.h>"
C.include "<chipmunk_structs.h>"

defaultHandler :: CollisionHandler
defaultHandler = CollisionHandler (Wildcard 0) Nothing Nothing Nothing Nothing


whenOrFalse :: (Applicative f) => Bool -> f Bool -> f Bool 
whenOrFalse a c
    | a = c
    | otherwise = pure False


-- | A 'CollideHandler' is a callback function that can possibly handle collisions.
-- If running the 'CollideHandler' returns 'True', it was able to handle this collision.
-- If it returns 'False', it could not, probably because one of the entities involved
-- didn't have the proper components.
--
-- 'CollideHandlers' are almost always created with 'mapMCollideHandler' or 'mapCollideHandler'
-- and are generally combined with 'collideHandlerOr' or 'collideHandlerAnd'.
-- However, it is also possible to create 'CollideHandlers' that simply do some other checking.
type CollideHandler w m = (Collision -> SystemT w m Bool)

-- | Make a CollideHandler that behaves like a two-argument 'mapM'.
-- If the callback can be applied to the enties in 'collisionA' and 'collisionB', then
-- this will run the callback, map the results to those same entities, and then return 'True'.
-- If either entity did not have the component(s) needed to run the callback function,
-- it will instead return 'False' and not run the callback.
--
-- This function is useful to obtain 'CollideHandler's that can be chained with 'mapColliderOr'
-- and 'mapColliderAnd' to perform actions in response to detected collisions.
mapMCollideHandler :: forall lhs rhs lhsr rhsr w m.
                    ( Get w m lhs
                    , Get w m rhs
                    , Set w m lhsr
                    , Set w m rhsr)
                 => (Vec -> lhs -> rhs -> SystemT w m (lhsr, rhsr))
                 -> CollideHandler w m 
mapMCollideHandler cb c = do
    let lhs' = collisionA c
    let rhs' = collisionB c
    lhse <- exists lhs' (Proxy @lhs)
    whenOrFalse lhse $ do
        rhse <- exists rhs' (Proxy @rhs)
        whenOrFalse rhse $ do
          lhsc <- get lhs'
          rhsc <- get rhs'
          (lhsr, rhsr) <- cb (collisionNormal c) lhsc rhsc
          set lhs' lhsr
          set rhs' rhsr
          pure True

-- | 'makeMapM' collider, but with a pure callback instead of an effectful one.
mapCollideHandler :: forall lhs rhs lhsr rhsr w m.
                   ( Get w m lhs
                   , Get w m rhs
                   , Set w m lhsr
                   , Set w m rhsr)
                => (Vec -> lhs -> rhs -> (lhsr, rhsr))
                -> CollideHandler w m
mapCollideHandler = mapMCollideHandler . pure3
  where
    pure3 f a b c = pure $ f a b c

-- | Given two 'CollideHandlers', first try to run the first.
-- If that is unsuccessful, try the second instead.
collideHandlerOr :: (Monad m)
              => CollideHandler w m
              -> CollideHandler w m 
              -> CollideHandler w m
collideHandlerOr a b col = do
    e <- a col
    if e then
      pure True
    else
      b col

-- | Given two 'CollideHandlers', return a new 'CollideHandler' that runs both,
-- and is successful if either is successful.
collideHandlerAnd :: (Applicative m)
               => CollideHandler w m
               -> CollideHandler w m 
               -> CollideHandler w m
collideHandlerAnd a b col =
    (||) <$> a col <*> b col


postSolveCollideHandler :: CollideHandler w IO -> System w PostSolveCB
postSolveCollideHandler c = mkPostSolveCB inner
  where
    inner col = c col $> ()

mkBeginCB :: (Collision -> System w Bool) -> System w BeginCB
mkBeginCB sys = do
    w <- ask

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
    w <- ask

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

instance Has w IO Physics => Has w IO CollisionHandler where
  getStore = (cast :: Space Physics -> Space CollisionHandler) <$> getStore

instance ExplSet IO (Space CollisionHandler) where
  explSet sp@(Space _ _ _ hMap spcPtr) ety handler = do
    explDestroy sp ety
    hPtr <- newCollisionHandler spcPtr handler ety
    modifyIORef' hMap (M.insert ety (Record hPtr handler))

instance ExplDestroy IO (Space CollisionHandler) where
  explDestroy (Space _ _ _ hMap _) ety = do
    rd <- M.lookup ety <$> readIORef hMap
    forM_ rd$ \(Record c _) -> destroyCollisionHandler c >> modifyIORef' hMap (M.delete ety)

instance ExplMembers IO (Space CollisionHandler) where
  explMembers (Space _ _ _ hMap _) = U.fromList . M.keys <$> readIORef hMap

instance ExplGet IO (Space CollisionHandler) where
  explExists (Space _ _ _ hMap _) ety = M.member ety <$> readIORef hMap
  explGet (Space _ _ _ hMap _) ety = do
    Just (Record _ handler) <- M.lookup ety <$> readIORef hMap
    return handler

