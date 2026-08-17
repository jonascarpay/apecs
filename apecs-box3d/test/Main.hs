{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| Headless behavior tests for the Apecs.Box3D wrapper: components,
event globals, queries and the authoring-path validations, no
rendering.
-}
module Main (main) where

import Apecs
import Apecs.Box3D
import Box3D.MathTypes (transformIdentity)
import Box3D.World qualified as B3World
import Control.Exception (ErrorCall, try)
import Control.Monad (replicateM_)
import Data.List (find, isInfixOf, sort)
import Data.Maybe (isJust)
import Data.Vector.Storable qualified as VS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))

makeWorld "World" [''Physics]

main :: IO ()
main =
  -- Each test drives its own engine world, and Box3D worlds contend on
  -- process-global solver state — stepping two concurrently corrupts it
  -- (an intermittent solver-set assertion / SIGILL). Tasty parallelises
  -- test cases by default under the threaded RTS, so pin the suite to one
  -- thread; a real app steps a single world from one thread and is fine.
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "Apecs.Box3D"
        [ testGroup
            "authoring validation"
            [ testCase "meshFromData rejects out-of-range indices" meshRejectsBadIndex
            , testCase "meshFromData rejects a ragged index count" meshRejectsRaggedIndices
            , testCase "heightFieldFromData rejects mismatched sample counts" heightFieldRejectsBadSizes
            , testCase "heightFieldFromData accepts a matching grid" heightFieldAcceptsGoodSizes
            , testCase "compoundFromChildren rejects multi-material meshes" compoundRejectsMultiMaterial
            , testCase "compoundFromChildren accepts single-material meshes" compoundAcceptsSingleMaterial
            ]
        , testCase "gravity pulls a dynamic body down" gravityPullsDown
        , testCase "begin-touch carries manifolds and pairs with its end event" contactLifecycle
        , testCase "containsPointQuery is exact for hulls, silent for meshes" containsPointHullVsMesh
        , testCase "segment and AABB queries find the shapes that are there" basicQueries
        , testCase "overlapQuery with a created shape's own Geometry reports its body" overlapRoundTrip
        , testCase "sweepQuery reports hits nearest-first" sweepOrdering
        , testCase "joint tuning survives a Joint re-set" jointTuningSurvivesReset
        , testCase "destroying a body drops its dependents' records" bodyDestroyDropsDeps
        , testCase "initPhysicsWith builds the store around a custom WorldDef" customWorldDef
        , testCase "moveCharacter reaches its target in free space" moverFreeSpace
        , testCase "debugDrawCommands records overlays but no shapes without debug-shape callbacks" debugDrawRecords
        ]

run :: SystemT World IO a -> IO a
run sys = initWorld >>= runSystem (sys <* destroyPhysics)

dT :: Float
dT = 1 / 60

{- | Step up to @n@ times, probing after each step; the first 'Just'
wins.
-}
stepUntil :: Int -> SystemT World IO (Maybe a) -> SystemT World IO (Maybe a)
stepUntil n probe
  | n <= 0 = pure Nothing
  | otherwise = do
      stepPhysics dT
      r <- probe
      maybe (stepUntil (n - 1) probe) (pure . Just) r

-- | A static body with a shape, at a position.
staticShape :: WVec -> Geometry -> SystemT World IO Entity
staticShape p geo = do
  body <- newEntity (StaticBody, Position p)
  newEntity_ (Shape body geo)
  pure body

expectError :: (Show a) => String -> IO a -> Assertion
expectError needle act = do
  r <- try @ErrorCall act
  case r of
    Right x -> assertFailure ("expected an error mentioning " <> show needle <> ", got " <> show x)
    Left e ->
      assertBool
        ("error mentions " <> show needle <> ": " <> show e)
        (needle `isInfixOf` show e)

triangle :: VS.Vector Vec3
triangle = VS.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 0 0 1]

meshRejectsBadIndex :: Assertion
meshRejectsBadIndex =
  expectError "index out of bounds" $
    meshFromData triangle (VS.fromList [0, 1, 7]) Nothing defaultMeshOptions

meshRejectsRaggedIndices :: Assertion
meshRejectsRaggedIndices =
  expectError "multiple of 3" $
    meshFromData triangle (VS.fromList [0, 1, 2, 2]) Nothing defaultMeshOptions

heightFieldRejectsBadSizes :: Assertion
heightFieldRejectsBadSizes = do
  expectError "height samples" $
    heightFieldFromData (VS.replicate 4 0) Nothing (Vec3 1 1 1) 3 3 defaultHeightFieldOptions
  expectError "cell materials" $
    heightFieldFromData (VS.replicate 9 0) (Just (VS.replicate 3 0)) (Vec3 1 1 1) 3 3 defaultHeightFieldOptions
  expectError "grid lines" $
    heightFieldFromData (VS.replicate 1 0) Nothing (Vec3 1 1 1) 1 1 defaultHeightFieldOptions

heightFieldAcceptsGoodSizes :: Assertion
heightFieldAcceptsGoodSizes = do
  -- an uncaught error fails the test and prints the real message
  _ <- heightFieldFromData (VS.replicate 9 0) (Just (VS.replicate 4 0)) (Vec3 1 1 1) 3 3 defaultHeightFieldOptions
  pure ()

compoundRejectsMultiMaterial :: Assertion
compoundRejectsMultiMaterial = do
  m <- gridMesh 4 4 1 3
  expectError "material slots" $
    compoundFromChildren [CompoundMesh m transformIdentity (Vec3 1 1 1)]

compoundAcceptsSingleMaterial :: Assertion
compoundAcceptsSingleMaterial = do
  m <- boxMesh vec3Zero (Vec3 1 1 1)
  _ <- compoundFromChildren [CompoundMesh m transformIdentity (Vec3 1 1 1)]
  pure ()

gravityPullsDown :: Assertion
gravityPullsDown = run $ do
  set global (Gravity (Vec3 0 (-10) 0))
  ball <- newEntity (DynamicBody, Position (Vec3 0 0 0))
  newEntity_ (Shape ball (GeoSphere vec3Zero 0.5))
  replicateM_ 30 (stepPhysics dT)
  Position (Vec3 _ y _) <- get ball
  liftIO $ assertBool ("fell below the start: y = " <> show y) (y < -0.5)

contactLifecycle :: Assertion
contactLifecycle = run $ do
  set global (Gravity (Vec3 0 (-10) 0))
  ground <- staticShape (Vec3 0 (-1) 0) (GeoBox vec3Zero (Vec3 10 1 10))
  ball <- newEntity (DynamicBody, Position (Vec3 0 1.5 0))
  newEntity_ (Shape ball (GeoSphere vec3Zero 0.5))
  let involvesBoth c =
        (c.bodyA, c.bodyB) `elem` [(ground, ball), (ball, ground)]
  mcol <- stepUntil 300 $ do
    Collisions cols <- get global
    pure (find involvesBoth cols)
  col <- maybe (liftIO (assertFailure "the ball never touched the ground")) pure mcol
  m <- case col.manifolds of
    [] -> liftIO (assertFailure "begin-touch carried no manifolds")
    m : _ -> pure m
  let Vec3 nx ny nz = m.normal
  liftIO $
    assertBool
      ("contact normal is vertical: " <> show (nx, ny, nz))
      (abs nx < 0.1 && abs ny > 0.9 && abs nz < 0.1)
  -- teleporting the ball away breaks the contact; the end event must
  -- pair with the begin event under the participants-only Eq
  set ball (Position (Vec3 100 100 100))
  mend <- stepUntil 10 $ do
    CollisionsEnd ends <- get global
    pure (find (== col) ends)
  liftIO $ assertBool "the end event pairs with the begin event via (==)" (isJust mend)

containsPointHullVsMesh :: Assertion
containsPointHullVsMesh = run $ do
  hullBody <- staticShape (Vec3 0 0 0) (GeoBox vec3Zero (Vec3 1 1 1))
  m <- liftIO (boxMesh vec3Zero (Vec3 1 1 1))
  meshBody <- staticShape (Vec3 10 0 0) (GeoMesh m (Vec3 1 1 1))
  stepPhysics dT
  inHull <- containsPointQuery (Vec3 0 0 0) everything
  inMesh <- containsPointQuery (Vec3 10 0 0) everything
  liftIO $ do
    inHull @?= [hullBody]
    -- documented limitation: the engine's exact point test reports no
    -- containment for non-convex shapes, so the mesh body never shows
    inMesh @?= []
  -- the mesh is still visible to the broad-phase and to rays
  boxed <- aabbQuery (Vec3 9 (-2) (-2)) (Vec3 11 2 2) everything
  liftIO $ boxed @?= [meshBody]

basicQueries :: Assertion
basicQueries = run $ do
  body <- staticShape (Vec3 0 0 0) (GeoBox vec3Zero (Vec3 1 1 1))
  stepPhysics dT
  hit <- segmentQuery (Vec3 (-5) 0 0) (Vec3 5 0 0) everything
  boxed <- aabbQuery (Vec3 (-2) (-2) (-2)) (Vec3 2 2 2) everything
  missed <- segmentQuery (Vec3 (-5) 5 0) (Vec3 5 5 0) everything
  liftIO $ do
    fmap (.body) hit @?= Just body
    boxed @?= [body]
    fmap (.body) missed @?= Nothing

overlapRoundTrip :: Assertion
overlapRoundTrip = run $ do
  let geo = GeoBox (Vec3 0.5 0 0) (Vec3 1 1 1)
  body <- staticShape (Vec3 0 0 0) geo
  stepPhysics dT
  hits <- overlapQuery geo everything
  misses <- overlapQuery (GeoSphere (Vec3 10 10 10) 0.1) everything
  liftIO $ do
    hits @?= [body]
    misses @?= []

sweepOrdering :: Assertion
sweepOrdering = run $ do
  near <- staticShape (Vec3 5 0 0) (GeoBox vec3Zero (Vec3 0.5 0.5 0.5))
  far <- staticShape (Vec3 10 0 0) (GeoBox vec3Zero (Vec3 0.5 0.5 0.5))
  stepPhysics dT
  hits <- sweepQuery (GeoSphere vec3Zero 0.2) (Vec3 20 0 0) everything
  liftIO $ do
    map (.body) hits @?= [near, far]
    let fracs = map (.fraction) hits
    assertBool ("fractions ascend: " <> show fracs) (fracs == sort fracs)

jointTuningSurvivesReset :: Assertion
jointTuningSurvivesReset = run $ do
  a <- newEntity (StaticBody, Position (Vec3 0 0 0))
  b <- newEntity (DynamicBody, Position (Vec3 2 0 0))
  newEntity_ (Shape b (GeoBox vec3Zero (Vec3 0.5 0.5 0.5)))
  j <- newEntity (Joint a b (HingeJoint (Vec3 1 0 0) (Vec3 0 0 1)))
  set j (MotorSpeed 3)
  set j (JointLimits (-1) 1)
  -- re-setting the Joint recreates the engine joint; the tuning must carry
  set j (Joint a b (HingeJoint (Vec3 1 0 0) (Vec3 0 0 1)))
  MotorSpeed speed <- get j
  JointLimits lo hi <- get j
  liftIO $ do
    speed @?= 3
    (lo, hi) @?= (-1, 1)

bodyDestroyDropsDeps :: Assertion
bodyDestroyDropsDeps = run $ do
  a <- newEntity (DynamicBody, Position (Vec3 0 0 0))
  sa <- newEntity (Shape a (GeoBox vec3Zero (Vec3 0.5 0.5 0.5)))
  b <- newEntity (DynamicBody, Position (Vec3 2 0 0))
  j <- newEntity (Joint a b (PivotJoint (Vec3 1 0 0)))
  destroy a (Proxy @Body)
  shapeThere <- exists sa (Proxy @Shape)
  jointThere <- exists j (Proxy @Joint)
  liftIO $ do
    shapeThere @?= False
    jointThere @?= False
  -- the surviving body's bookkeeping stays consistent: it can be
  -- destroyed too without tripping over the dead joint
  destroy b (Proxy @Body)
  bThere <- exists b (Proxy @Body)
  liftIO $ bThere @?= False

customWorldDef :: Assertion
customWorldDef = do
  wd <- B3World.defaultWorldDef
  -- the documented hand-built world value, in place of initWorld
  w <- World <$> initPhysicsWith wd{B3World.gravity = Vec3 0 (-3) 0} <*> explInit
  runSystem
    ( do
        Gravity g <- get global
        liftIO (g @?= Vec3 0 (-3) 0)
        destroyPhysics
    )
    w

moverFreeSpace :: Assertion
moverFreeSpace = run $ do
  MoverResult (Vec3 px py pz) _ <-
    moveCharacter (Vec3 0 (-0.3) 0) (Vec3 0 0.3 0) 0.3 (Vec3 0 0 0) (Vec3 1 0 0) (Vec3 1 0 0) everything
  liftIO $ do
    assertBool ("reaches the target x: " <> show px) (abs (px - 1) < 0.05)
    assertBool ("stays on the line: " <> show (py, pz)) (abs py < 0.05 && abs pz < 0.05)

debugDrawRecords :: Assertion
debugDrawRecords = run $ do
  a <- staticShape (Vec3 0 0 0) (GeoBox vec3Zero (Vec3 1 1 1))
  b <- newEntity (DynamicBody, Position (Vec3 3 0 0))
  newEntity_ (Shape b (GeoSphere vec3Zero 0.5))
  newEntity_ (Joint a b (HingeJoint (Vec3 1.5 0 0) (Vec3 0 0 1)))
  base <- liftIO defaultDebugDraw
  shapes <- debugDrawCommands base{drawShapes = 1} maxBound
  overlays <- debugDrawCommands base{drawShapes = 0, drawJoints = 1, drawBounds = 1} maxBound
  bare <- debugDrawCommands base{drawShapes = 0} maxBound
  liftIO $ do
    assertBool "shapes need the world's debug-shape callbacks" (not (any isShape shapes))
    assertBool "bounds when asked" (any isBounds overlays)
    assertBool "joint drawn as something" (length overlays > length bare)
    bare @?= []
  where
    isShape DrawShape{} = True
    isShape _ = False
    isBounds DrawBounds{} = True
    isBounds _ = False
