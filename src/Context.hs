{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Context where

import Linear.V2
import Apecs
import Apecs.Types
import Apecs.TH
import Data.Monoid ((<>))
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Ptr
import Language.C.Inline
import Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

phycsCtx = baseCtx <> funCtx <> ctx
  where ctx = mempty { ctxTypesTable = phycsTypesTable }

phycsTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
phycsTypesTable = Map.fromList
  [ (C.TypeName "cpSpace", [t| FrnSpace |])
  , (C.TypeName "cpBody", [t| Body |])
  , (C.TypeName "cpShape", [t| Shape |])
  ]

type Vec = V2 Double
type BVec = Vec
type WVec = Vec
data CollisionType
data CollisionGroup
data Collisionmask
data Transform

data Body = DynamicBody | KinematicBody | StaticBody deriving (Eq, Ord, Enum) -- TODO: Enum matchen met cpBodyType

newtype Position = Position WVec
newtype Velocity = Velocity WVec
newtype Force = Force Vec
newtype CenterOfGravity = CenterOfGravity BVec
newtype Mass = Mass Double
newtype Moment = Moment Double
newtype Angle = Angle Double
newtype AngularVelocity = AngularVelocity Double
newtype Sleeping = Sleeping Bool
-- TODO cpBodyGetRotation? cpvrotate(), cpvunrotate()
-- bodyToWorld :: Entity c -> BVec -> System w WVec
-- worldToBody :: ...

type Verts = [Vec]
data Shape = Circle BVec Double | Line Vec Vec Double | Poly Verts Transform Double
-- TODO: Line/poly vec space?
-- TODO Line neighbours?

newtype Sensor = Sensor Bool
newtype Elasticity = Elasticity Double -- 0 no bounce, 1 perfect bounce, TODO Bounded
newtype Friction = Friction Double -- Coulomb model
newtype SurfaceVelocity = SurfaceVelocity Vec
newtype ShapeCollisionType = ShapeCollisionType CollisionType
newtype FilterType = FilterType CollisionType -- TODO cpShapeFilter
-- shapeGetBB
-- need to be removed when body is removed

data FrnSpace
type EntityMap = M.IntMap (Ptr Body, [Ptr Shape])
data Space c = Space (IORef EntityMap) (ForeignPtr FrnSpace)
newtype Iterations = Iterations Int
newtype Gravity = Gravity Vec
newtype Damping = Damping Double
newtype IdleSpeedThreshold = IdleSpeedThreshold Double
newtype SleepIdleTime = SleepIdleTime Double
newtype CollisionSlop = CollisionSlop Double
newtype CollisionBias = CollisionBias Double
-- useSpatialHash :: dim, bin size -> System

