{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Shape where

import           Apecs.Types
import           Control.Monad
import           Data.Bits
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline  as C
import qualified Language.C.Types   as C
import           Linear.V2

import           Instances
import           Types

C.context phycsCtx
C.include "<chipmunk.h>"

instance Component Shapes where
  type Storage Shapes = Space Shapes

instance Has w Body => Has w Shapes where
  getStore = (cast :: Space Body -> Space Shapes) <$> getStore

instance Store (Space Shapes) where
  type Stores (Space Shapes) = Shapes
  type SafeRW (Space Shapes) = Maybe Shapes
  initStore = error "Initializing space from non-body store"
  explDestroy _ _ = return ()
  {-explMembers s = explMembers (cast s :: Space Body)-}
  {-explExists s ety = explExists (cast s :: Space Body) ety-}
  explSetMaybe = defaultSetMaybe

  -- TODO: get/set shapes

maskAll, maskNone :: Bitmask
maskAll  = complement zeroBits
maskNone = zeroBits
maskList :: [Int] -> Bitmask
maskList = foldr (flip setBit) maskNone

-- Shape
newShape :: SpacePtr -> Ptr Body -> ShapeType -> IO (Ptr Shape)
newShape spacePtr' bodyPtr shape = withForeignPtr spacePtr' (go shape)
  where

    go (Circle (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> radius)) spacePtr = [C.block| cpShape* {
      const cpVect vec = { $(double x), $(double y) };
      return cpCircleShapeNew($(cpBody* bodyPtr), $(double radius), vec); } |]

    go (Segment (V2 (realToFrac -> xa) (realToFrac -> ya))
                (V2 (realToFrac -> xb) (realToFrac -> yb))
                (realToFrac -> radius)
       ) spacePtr = [C.block| cpShape* {
       const cpVect va = { $(double xa), $(double ya) };
       const cpVect vb = { $(double xb), $(double yb) };
       return cpSegmentShapeNew($(cpBody* bodyPtr), va, vb, $(double radius)); } |]

    go (Convex ((fmap.fmap) realToFrac -> verts)
               (realToFrac -> radius)
       ) spacePtr =
         do let n = fromIntegral$ length verts
            vecs <- [C.exp| cpVect* { malloc($(int n)*sizeof(cpVect*)) } |]
            forM_ (zip verts [1..]) (\(V2 x y, i) -> [C.block| void {
                $(cpVect* vecs)[$(int i)].x = $(double x);
                $(cpVect* vecs)[$(int i)].y = $(double y);
              }|])

            [C.block| cpShape* {
              cpTransform trans = { 1, 0, 0, 1, 0, 0 };
              return cpPolyShapeNew($(cpBody* bodyPtr), $(int n), $(cpVect* vecs), trans, $(double radius));
            }|]

destroyShape :: Ptr Shape -> IO ()
destroyShape shapePtr = [C.block| void {
  cpShapeDestroy ($(cpShape* shapePtr));
  cpShapeFree ($(cpShape* shapePtr)); }|]

