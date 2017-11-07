{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
  , (C.TypeName "cpVect", [t| FrnVec |])
  ]

type Vec = V2 Double
type BVec = Vec
type WVec = Vec
data CollisionType
data CollisionGroup
data Collisionmask

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
data ShapeType = Circle BVec Double
               | Segment Vec Vec Double
               | Convex Verts Double

data ShapeProperties = ShapeProperties
  { sensor :: Bool
  , elasticity :: Double
  , friction :: Double
  , surfaceVelocity :: Vec
    , group :: Group
  , filter :: Filter
  }
data CollisionFilter

type Group = CUInt
type Filter = forall a. (Eq a, Enum a) => [a]

data Shape = Shape [(ShapeType, ShapeProperties)]



-- TODO Segment neighbours?


data FrnSpace
data FrnVec

type SpacePtr = ForeignPtr FrnSpace
type EntityMap = M.IntMap (Ptr Body, [(ShapeType, Ptr Shape)])
data Space c = Space (IORef EntityMap) SpacePtr
newtype Iterations = Iterations Int
newtype Gravity = Gravity Vec
newtype Damping = Damping Double
newtype IdleSpeedThreshold = IdleSpeedThreshold Double
newtype SleepIdleTime = SleepIdleTime Double
newtype CollisionSlop = CollisionSlop Double
newtype CollisionBias = CollisionBias Double
-- useSpatialHash :: dim, bin size -> System

