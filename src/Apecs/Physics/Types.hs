{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Apecs.Physics.Types where

import           Apecs
import           Apecs.Types
import           Data.Bits
import           Data.Char                 (intToDigit)
import qualified Data.IntMap               as M
import           Data.IORef
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types          as C
import qualified Language.Haskell.TH       as TH
import           Linear.V2
import           Numeric                   (showIntAtBase)

phycsCtx :: Context
phycsCtx = baseCtx <> funCtx <> ctx
  where ctx = mempty { ctxTypesTable = phycsTypesTable }

phycsTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
phycsTypesTable = Map.fromList
  [ (C.TypeName "cpSpace", [t| FrnSpace |])
  , (C.TypeName "cpBody", [t| Body |])
  , (C.TypeName "cpShape", [t| Shape |])
  , (C.TypeName "cpVect", [t| FrnVec |])
  ]

data Physics -- Dummy component that adds a physics system to a World

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
newtype Mass = Mass Double deriving (Eq, Show)
newtype Moment = Moment Double deriving (Eq, Show)
newtype Angle = Angle Double deriving (Eq, Show)
newtype AngularVelocity = AngularVelocity Double
newtype Sleeping = Sleeping Bool

data Shape = Shape ShapeType ShapeProperties
           | Compound [Shape]

instance Monoid Shape where
  mempty = Compound []
  Compound as `mappend` Compound bs = Compound (as `mappend` bs)
  Compound as `mappend` sh          = Compound (sh : as)
  sh          `mappend` Compound as = Compound (sh : as)
  sha         `mappend` shb         = Compound [sha, shb]

type Verts = [Vec]
data ShapeType = Circle BVec Double
               | Segment Vec Vec Double
               | Convex Verts Double
               deriving (Eq, Show)

data ShapeProperties = ShapeProperties
  { sensor          :: Bool
  , elasticity      :: Double
  , mass            :: SMass
  , friction        :: Double
  , surfaceVelocity :: Vec
  , filter          :: CollisionFilter
  }
  deriving (Eq, Show)

data CollisionFilter = CollisionFilter
  { group      :: Group
  , categories :: Bitmask
  , mask       :: Bitmask
  } deriving (Eq, Show)

data SMass = SMass Double | SDensity Double deriving (Eq, Show)
type Group = CUInt
newtype Bitmask = Bitmask CUInt deriving (Eq, Bits)
instance Show Bitmask where
  show (Bitmask mask) = "Bitmask " ++ showIntAtBase 2 intToDigit mask ""

data FrnSpace
data FrnVec

data Space c = Space (IORef BodyMap) SpacePtr
type SpacePtr = ForeignPtr FrnSpace

data BodyRecord = BodyRecord
  { bodyPtr   :: Ptr Body
  , shapes    :: Shape
  , shapePtrs :: [Ptr Shape]
  }
type BodyMap = M.IntMap BodyRecord

newtype Iterations = Iterations Int
newtype Gravity = Gravity Vec
newtype Damping = Damping Double
newtype IdleSpeedThreshold = IdleSpeedThreshold Double
newtype SleepIdleTime = SleepIdleTime Double
newtype CollisionSlop = CollisionSlop Double
newtype CollisionBias = CollisionBias Double

instance Cast Space where
  cast (Space c ref) = Space c ref

