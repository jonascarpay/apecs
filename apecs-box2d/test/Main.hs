{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| Headless behavior tests for the Apecs.Box2D wrapper: components,
event globals and queries against a real engine world, no rendering.
-}
module Main (main) where

import Apecs
import Box2D.World qualified as B2World
import Control.Monad (replicateM_)
import Data.List (find, sort)
import Data.Maybe (isJust)
import Data.Vector.Storable qualified as VS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))

import Apecs.Box2D

makeWorld "World" [''Physics]

main :: IO ()
main =
  -- Each test drives its own engine world, and Box2D worlds contend on
  -- process-global solver state — stepping two concurrently corrupts it
  -- (an intermittent b2_awakeSet assertion / SIGILL). Tasty parallelises
  -- test cases by default under the threaded RTS, so pin the suite to one
  -- thread; a real app steps a single world from one thread and is fine.
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "Apecs.Box2D"
        [ testCase "gravity pulls a dynamic body down" gravityPullsDown
        , testCase "begin-touch carries a manifold and pairs with its end event" contactLifecycle
        , testCase "point and AABB queries find the shapes that are there" pointAndAabbQueries
        , testCase "overlapQuery with a created shape's own Geometry reports its body" overlapRoundTrip
        , testCase "sweepQuery reports hits nearest-first" sweepOrdering
        , testCase "segmentQuery reports the closest body" segmentClosest
        , testCase "chain segments resolve to the chain entity in queries" chainVisibility
        , testCase "joint tuning survives a Joint re-set" jointTuningSurvivesReset
        , testCase "destroying a body drops its dependents' records" bodyDestroyDropsDeps
        , testCase "initPhysicsWith builds the store around a custom WorldDef" customWorldDef
        , testCase "moveCharacter reaches its target in free space" moverFreeSpace
        , testCase "moveCharacter stops at a wall and clips velocity" moverWall
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

gravityPullsDown :: Assertion
gravityPullsDown = run $ do
  set global (Gravity (Vec2 0 (-10)))
  ball <- newEntity (DynamicBody, Position (Vec2 0 0))
  newEntity_ (Shape ball (GeoCircle vec2Zero 0.5))
  replicateM_ 30 (stepPhysics dT)
  Position (Vec2 _ y) <- get ball
  liftIO $ assertBool ("fell below the start: y = " <> show y) (y < -0.5)

contactLifecycle :: Assertion
contactLifecycle = run $ do
  set global (Gravity (Vec2 0 (-10)))
  ground <- staticShape (Vec2 0 (-1)) (GeoBox 10 1)
  ball <- newEntity (DynamicBody, Position (Vec2 0 1.5))
  newEntity_ (Shape ball (GeoCircle vec2Zero 0.5))
  let involvesBoth c =
        (c.bodyA, c.bodyB) `elem` [(ground, ball), (ball, ground)]
  mcol <- stepUntil 300 $ do
    Collisions cols <- get global
    pure (find involvesBoth cols)
  col <- maybe (liftIO (assertFailure "the ball never touched the ground")) pure mcol
  m <- maybe (liftIO (assertFailure "begin-touch carried no manifold")) pure col.manifold
  let Vec2 nx ny = m.normal
  liftIO $ assertBool ("contact normal is vertical: " <> show (nx, ny)) (abs nx < 0.1 && abs ny > 0.9)
  -- teleporting the ball away breaks the contact; the end event must
  -- pair with the begin event under the participants-only Eq
  set ball (Position (Vec2 100 100))
  mend <- stepUntil 10 $ do
    CollisionsEnd ends <- get global
    pure (find (== col) ends)
  liftIO $ assertBool "the end event pairs with the begin event via (==)" (isJust mend)

pointAndAabbQueries :: Assertion
pointAndAabbQueries = run $ do
  body <- staticShape (Vec2 0 0) (GeoBox 1 1)
  stepPhysics dT
  inside <- containsPointQuery (Vec2 0.5 0.5) everything
  outside <- containsPointQuery (Vec2 3 3) everything
  boxed <- aabbQuery (Vec2 (-2) (-2)) (Vec2 2 2) everything
  nearby <- pointQuery (Vec2 1.5 0) 1 everything
  liftIO $ do
    inside @?= [body]
    outside @?= []
    boxed @?= [body]
    nearby @?= [body]

overlapRoundTrip :: Assertion
overlapRoundTrip = run $ do
  let geo = GeoOffsetBox 1 1 (Vec2 0.5 0) 0.7
  body <- staticShape (Vec2 0 0) geo
  stepPhysics dT
  hits <- overlapQuery geo everything
  misses <- overlapQuery (GeoCircle (Vec2 10 10) 0.1) everything
  liftIO $ do
    hits @?= [body]
    misses @?= []

sweepOrdering :: Assertion
sweepOrdering = run $ do
  near <- staticShape (Vec2 5 0) (GeoBox 0.5 0.5)
  far <- staticShape (Vec2 10 0) (GeoBox 0.5 0.5)
  stepPhysics dT
  hits <- sweepQuery (GeoCircle vec2Zero 0.2) (Vec2 20 0) everything
  liftIO $ do
    map (.body) hits @?= [near, far]
    let fracs = map (.fraction) hits
    assertBool ("fractions ascend: " <> show fracs) (fracs == sort fracs)

segmentClosest :: Assertion
segmentClosest = run $ do
  body <- staticShape (Vec2 0 0) (GeoBox 1 1)
  _behind <- staticShape (Vec2 4 0) (GeoBox 1 1)
  stepPhysics dT
  hit <- segmentQuery (Vec2 (-5) 0) (Vec2 5 0) everything
  liftIO $ fmap (.body) hit @?= Just body

chainVisibility :: Assertion
chainVisibility = run $ do
  body <- newEntity (StaticBody, Position (Vec2 0 0))
  let pts = VS.fromList [Vec2 (-3) 0, Vec2 (-1) 0, Vec2 1 0, Vec2 3 0]
  chain <- newEntity (Chain body pts False)
  stepPhysics dT
  -- rays report the CHAIN entity in the shape slot; chain segments are
  -- one-sided, so probe from both sides
  above <- segmentQueryAll (Vec2 0 2) (Vec2 0 (-2)) everything
  below <- segmentQueryAll (Vec2 0 (-2)) (Vec2 0 2) everything
  let chainHits = filter ((== chain) . (.shape)) (above <> below)
  liftIO $ do
    assertBool "a ray across the chain reports the chain entity" (not (null chainHits))
    assertBool "the chain's body entity rides along" (all ((== body) . (.body)) chainHits)
  -- containsPointQuery can never report a chain (documented): the
  -- engine's exact point test has no chain-segment case
  onChain <- containsPointQuery (Vec2 0 0) everything
  liftIO $ onChain @?= []

moverCapsule :: (BVec, BVec, Float)
moverCapsule = (Vec2 0 (-0.3), Vec2 0 0.3, 0.3)

jointTuningSurvivesReset :: Assertion
jointTuningSurvivesReset = run $ do
  a <- newEntity (StaticBody, Position (Vec2 0 0))
  b <- newEntity (DynamicBody, Position (Vec2 2 0))
  newEntity_ (Shape b (GeoBox 0.5 0.5))
  j <- newEntity (Joint a b (PivotJoint (Vec2 1 0)))
  set j (MotorSpeed 3)
  set j (JointLimits (-1) 1)
  -- re-setting the Joint recreates the engine joint; the tuning must carry
  set j (Joint a b (PivotJoint (Vec2 1 0)))
  MotorSpeed speed <- get j
  JointLimits lo hi <- get j
  liftIO $ do
    speed @?= 3
    (lo, hi) @?= (-1, 1)

bodyDestroyDropsDeps :: Assertion
bodyDestroyDropsDeps = run $ do
  a <- newEntity (DynamicBody, Position (Vec2 0 0))
  sa <- newEntity (Shape a (GeoBox 0.5 0.5))
  b <- newEntity (DynamicBody, Position (Vec2 2 0))
  j <- newEntity (Joint a b (PivotJoint (Vec2 1 0)))
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
  wd <- B2World.defaultWorldDef
  -- the documented hand-built world value, in place of initWorld
  w <- World <$> initPhysicsWith wd{B2World.gravity = Vec2 0 (-3)} <*> explInit
  runSystem
    ( do
        Gravity g <- get global
        liftIO (g @?= Vec2 0 (-3))
        destroyPhysics
    )
    w

moverFreeSpace :: Assertion
moverFreeSpace = run $ do
  let (c1, c2, r) = moverCapsule
  MoverResult (Vec2 px py) _ <- moveCharacter c1 c2 r (Vec2 0 0) (Vec2 1 0) (Vec2 1 0) everything
  liftIO $ do
    assertBool ("reaches the target x: " <> show px) (abs (px - 1) < 0.05)
    assertBool ("stays on the line: " <> show py) (abs py < 0.05)

moverWall :: Assertion
moverWall = run $ do
  _wall <- staticShape (Vec2 3 0) (GeoBox 0.2 5)
  stepPhysics dT
  let (c1, c2, r) = moverCapsule
  MoverResult (Vec2 px _) (Vec2 vx _) <- moveCharacter c1 c2 r (Vec2 0 0) (Vec2 6 0) (Vec2 6 0) everything
  liftIO $ do
    -- wall face is at x = 2.8; the capsule radius keeps the mover
    -- origin at least 0.3 before it
    assertBool ("stops before the wall: x = " <> show px) (px < 2.6)
    assertBool ("into-wall velocity is clipped: vx = " <> show vx) (vx < 0.5)
