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

module Apecs.Physics.Query where

import           Apecs
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Foreign.C.Types
import           Foreign.ForeignPtr    (withForeignPtr)
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Language.C.Inline     as C
import           Linear.V2

import           Apecs.Physics.Space   ()
import           Apecs.Physics.Types

C.context phycsCtx
C.include "<chipmunk.h>"


-- cpFloat cpShapeNearestPointQuery(cpShape *shape, cpVect p, cpPointQueryInfo *out)
-- cpShape *cpSpacePointQueryNearest(cpSpace *space, cpVect point, cpFloat maxDistance, cpShapeFilter filter, cpPointQueryInfo *out)

pointQuery :: (MonadIO m, Has w m Physics) => WVec -> Double -> CollisionFilter -> SystemT w m (Maybe PointQueryResult)
pointQuery (fmap realToFrac -> V2 px py) (realToFrac -> maxDistance) (CollisionFilter gr (Bitmask cs) (Bitmask mk)) = do
  Space _ _ _ _ spcPtr :: Space Physics <- getStore
  liftIO $ alloca $ \pq -> do
    withForeignPtr spcPtr $ \space -> [C.block| void {
      cpSpacePointQueryNearest
        ( $(cpSpace *space)
        , cpv($(double px), $(double py))
        , $(double maxDistance)
        , cpShapeFilterNew($(unsigned int gr), $(unsigned int cs), $(unsigned int mk))
        , $(cpPointQueryInfo *pq));
      }|]
    res <- peek pq
    if unEntity (pqShape res) == -1
       then return Nothing
       else return (Just res)

instance Storable PointQueryResult where
  sizeOf ~_ = 48 -- sizeOf (undefined :: Ptr Shape) + sizeOf (undefined :: CDouble) + 2*sizeOf (undefined :: V2 CDouble)
  alignment ~_ = 8
  peek ptr = do
    sPtr :: Ptr Shape <- peekByteOff ptr 0
    s <- [C.block| intptr_t {
            cpShape *shape = $(cpShape *sPtr);
            if (shape==NULL) {
              return -1;
            } else {
              return (intptr_t) cpShapeGetUserData(shape);
            } }|]
    p :: V2 CDouble <- peekByteOff ptr 8
    d :: CDouble <- peekByteOff ptr 24
    g :: V2 CDouble <- peekByteOff ptr 32
    return $ PointQueryResult (Entity . fromIntegral $ s) (realToFrac <$> p) (realToFrac d) (realToFrac <$> g)
  poke = undefined
