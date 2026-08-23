{-# LANGUAGE DuplicateRecordFields #-}

{-| Apecs glue for the Box3D physics engine, the 3D sibling of
apecs-box2d (both modelled on apecs-physics).

Add 'Physics' to your world to get a Box3D world. Giving an entity a
'Body' component creates an engine body and unlocks its sub-components
('Position', 'Velocity', 'Rotation', ...), which read and write the
engine directly instead of mirroring state into apecs stores. Shapes
hang off a body entity through the 'Shape' component. Setting a
sub-component on an entity that has no 'Body' (or 'Shape') is a silent
no-op.

Vectors are Box3D's native single-precision 'Vec3' and rotations are
quaternions ('Quat'); convert at the boundary. 'Elasticity' is Box3D
restitution.

The raw engine is reachable through 'B3BodyId', 'B3ShapeId' and
'getWorldId' together with the "Box3D" modules. The wrapper owns
the engine's user-index channel: it stamps every body, shape and
joint with its entity id and resolves events and queries through
it, so raw-API users must not call @setUserIndex@ on
wrapper-created objects — and objects created directly through the
raw API are invisible to the wrapper's components, events and
queries.
-}
module Apecs.Box3D
  ( -- * World
    Physics
  , B3Space
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
  , Rotation (..)
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
  , SleepEnabled (..)
  , SleepThreshold (..)
  , CenterOfMass (..)
  , RotationalInertia (..)
  , BodyName (..)
  , B3BodyId (..)

    -- * Shape
  , Geometry (..)
  , Mesh
  , HeightField
  , Hull
  , Compound
  , Shape (..)
  , Density (..)
  , Friction (..)
  , Elasticity (..)
  , CollisionFilter (..)
  , Sensor (..)
  , Filter (..)
  , B3ShapeId (..)

    -- * Static geometry
  , meshFromData
  , MeshOptions (..)
  , defaultMeshOptions
  , heightFieldFromData
  , HeightFieldOptions (..)
  , defaultHeightFieldOptions
  , boxMesh
  , hollowBoxMesh
  , platformMesh
  , gridMesh
  , torusMesh
  , waveMesh
  , gridHeightField
  , waveHeightField
  , rockHull
  , coneHull
  , cylinderHull
  , CompoundChild (..)
  , compoundFromChildren

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
  , B3JointId (..)

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
  , Vec3 (..)
  , vec3Zero
  , Quat (..)
  , quatIdentity
  , Matrix3 (..)
  , BVec
  , WVec
  ) where

import Box3D.Authoring (HeightFieldOptions (..), MeshOptions (..), defaultHeightFieldOptions, defaultMeshOptions)
import Box3D.MathTypes (Matrix3 (..), Quat (..), Vec3 (..), quatIdentity, vec3Zero)
import Box3D.Shape (Filter (..))

import Apecs.Box3D.Body
import Apecs.Box3D.Collision
import Apecs.Box3D.Debug
import Apecs.Box3D.Geometry
import Apecs.Box3D.Joint
import Apecs.Box3D.Mover
import Apecs.Box3D.Query
import Apecs.Box3D.Recording
import Apecs.Box3D.Shape
import Apecs.Box3D.Space
import Apecs.Box3D.Types
