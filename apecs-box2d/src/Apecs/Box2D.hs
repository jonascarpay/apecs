{-# LANGUAGE DuplicateRecordFields #-}

{-| Apecs glue for the Box2D physics engine, modelled on apecs-physics.

Add 'Physics' to your world to get a Box2D world. Giving an entity a
'Body' component creates an engine body and unlocks its sub-components
('Position', 'Velocity', 'Angle', ...), which read and write the engine
directly instead of mirroring state into apecs stores. Shapes hang off
a body entity through the 'Shape' component, like in apecs-physics.
As in apecs-physics, setting a sub-component on an entity that has no
'Body' (or 'Shape') is a silent no-op.

Deviations from apecs-physics: vectors are Box2D's native
single-precision 'Vec2' (convert to your vector library of choice at
the boundary); 'Elasticity' is Box2D restitution; 'Substeps' replaces
@Iterations@.

The raw engine is reachable through 'B2BodyId', 'B2ShapeId' and
'getWorldId' together with the "Box2D" modules. The wrapper owns
the engine's user-index channel: it stamps every body, shape and
joint with its entity id and resolves events and queries through
it, so raw-API users must not call @setUserIndex@ on
wrapper-created objects — and objects created directly through the
raw API are invisible to the wrapper's components, events and
queries.
-}
module Apecs.Box2D
  ( -- * World
    Physics
  , B2Space
  , Gravity (..)
  , earthGravity
  , Substeps (..)
  , SleepingEnabled (..)
  , ContinuousEnabled (..)
  , HitEventThreshold (..)
  , RestitutionThreshold (..)
  , MaximumLinearSpeed (..)
  , WorkerCount (..)
  , initPhysicsWith
  , initPhysicsFrom
  , stepPhysics
  , destroyPhysics
  , explode
  , getWorldId

    -- * Body
  , Body (..)
  , Position (..)
  , Velocity (..)
  , Angle (..)
  , AngularVelocity (..)
  , BodyMass (..)
  , Force (..)
  , Torque (..)
  , LinearImpulse (..)
  , AngularImpulse (..)
  , ForceAt (..)
  , ImpulseAt (..)
  , TargetTransform (..)
  , LinearDamping (..)
  , AngularDamping (..)
  , GravityScale (..)
  , BulletBody (..)
  , BodyEnabled (..)
  , Awake (..)
  , MotionLocks (..)
  , FixedRotation (..)
  , SleepEnabled (..)
  , SleepThreshold (..)
  , CenterOfMass (..)
  , RotationalInertia (..)
  , BodyName (..)
  , B2BodyId (..)

    -- * Shape
  , Geometry (..)
  , Shape (..)
  , Density (..)
  , Friction (..)
  , Elasticity (..)
  , CollisionFilter (..)
  , Sensor (..)
  , Filter (..)
  , B2ShapeId (..)
  , Chain (..)
  , B2ChainId (..)

    -- * Joint
  , JointSpec (..)
  , Joint (..)
  , MotorSpeed (..)
  , MotorMaxTorque (..)
  , MotorMaxForce (..)
  , JointLimits (..)
  , CollideConnected (..)
  , JointForce (..)
  , JointTorque (..)
  , JointForceThreshold (..)
  , JointTorqueThreshold (..)
  , B2JointId (..)

    -- * Queries
  , RayHit (..)
  , everything
  , segmentQuery
  , segmentQueryAll
  , aabbQuery
  , pointQuery
  , containsPointQuery
  , overlapQuery
  , sweepQuery

    -- * Character mover
  , MoverResult (..)
  , moveCharacter

    -- * Recording
  , Recording
  , newRecording
  , destroyRecording
  , startRecording
  , stopRecording
  , saveRecording
  , loadRecording
  , validateRecording

    -- * Snapshot
  , Snapshot
  , snapshotWorld
  , restoreWorld

    -- * Debug drawing
  , debugDraw
  , debugDrawCommands
  , Draw (..)
  , noDraw
  , Cmd (..)
  , DebugDraw (..)
  , defaultDebugDraw

    -- * Collisions
  , Collision (..)
  , ContactManifold (..)
  , Collisions (..)
  , CollisionsEnd (..)
  , Impact (..)
  , Impacts (..)
  , SensorEvent (..)
  , SensorEvents (..)
  , SensorEventsEnd (..)
  , JointEvents (..)
  , BodyMove (..)
  , Moved (..)

    -- * Vectors
  , Vec2 (..)
  , vec2Zero
  , BVec
  , WVec
  ) where

import Box2D.MathTypes (Vec2 (..), vec2Zero)
import Box2D.Shape (Filter (..))

import Apecs.Box2D.Body
import Apecs.Box2D.Collision
import Apecs.Box2D.Debug
import Apecs.Box2D.Geometry
import Apecs.Box2D.Joint
import Apecs.Box2D.Mover
import Apecs.Box2D.Query
import Apecs.Box2D.Recording
import Apecs.Box2D.Shape
import Apecs.Box2D.Space
import Apecs.Box2D.Types
