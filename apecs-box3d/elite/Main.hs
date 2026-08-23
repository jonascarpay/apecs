{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| Elitelite: a kinetic chase through an asteroid field. One Courier
runs, two Eagles pursue, and everything is autopiloted: each ship is a
Box3D rigid body flown by a small PD controller (torque toward the
desired direction, thrust along the nose). The guns fire real dynamic
bodies with continuous collision detection ('BulletBody'), so hits are
resolved by the engine as momentum transfer; the step's 'Impacts' then
despawn spent slugs and throw explosion flashes at the contact points.
Rendered as flat-shaded solids with apecs-gloss-3d. Click to scramble
another Eagle.

The elite demo also has a headless verification and recording mode
that simulates at a fixed 60 Hz and exports exact frames via
gloss-export (no window, so no compositor throttling in the way),
logging the explosion pieces in view per frame. Stride 1 exports every
frame, ready for `ffmpeg -framerate 60`. Runs are deterministic: the
seed is printed, and passing it back reproduces the encounter bit for
bit.

@
apecs-box3d-elite -- film <warmup-seconds> <frames> <stride> <prefix> [seed]
@
-}
module Main (main) where

import Apecs
import Apecs.Gloss
import Apecs.Gloss3D
import Control.Monad (forM, forM_, replicateM, replicateM_, when)
import Data.Vector.Storable qualified as VS
import Graphics.Gloss.Export.PNG (exportPictureToPNG)
import Linear (Quaternion (..), V3 (..), cross, dot, norm, normalize, quadrance, (*^), (^*), (^/))
import Linear qualified
import System.Environment (getArgs)
import System.Random (mkStdGen, randomRIO, setStdGen)

import Apecs.Box3D

-- * Components

-- | How an entity is rendered; physics state comes from its body.
data Look
  = LookSolid Color Solid
  | LookDot Color Float

instance Component Look where
  type Storage Look = Map Look

-- | Which autopilot flies this ship.
data Pilot = Courier | Eagle

instance Component Pilot where
  type Storage Pilot = Map Pilot

-- | Seconds until the gun may fire again.
newtype Gun = Gun Float

instance Component Gun where
  type Storage Gun = Map Gun

-- | Seconds of flight left before the slug despawns.
newtype Bullet = Bullet Float

instance Component Bullet where
  type Storage Bullet = Map Bullet

-- | Marks an asteroid; carries its rough radius for avoidance.
newtype Roid = Roid Float

instance Component Roid where
  type Storage Roid = Map Roid

-- | An explosion flash: position, magnitude, age. Pure VFX, no body.
data Boom = Boom (V3 Float) Float Float

instance Component Boom where
  type Storage Boom = Map Boom

-- | Elapsed scene time, for the Courier's jinking.
newtype Time = Time Float

instance Semigroup Time where
  _ <> b = b

instance Monoid Time where
  mempty = Time 0

instance Component Time where
  type Storage Time = Global Time

-- | Smoothed chase-camera state: eye and target.
data ChaseCam = ChaseCam (V3 Float) (V3 Float)

instance Semigroup ChaseCam where
  _ <> b = b

instance Monoid ChaseCam where
  mempty = ChaseCam (V3 0 3 (-14)) (V3 0 0 0)

instance Component ChaseCam where
  type Storage ChaseCam = Global ChaseCam

-- | Background stars: direction, brightness, size.
newtype Stars = Stars [(V3 Float, Float, Float)]

instance Semigroup Stars where
  _ <> b = b

instance Monoid Stars where
  mempty = Stars []

instance Component Stars where
  type Storage Stars = Global Stars

makeWorld
  "World"
  [ ''Physics
  , ''Look
  , ''Pilot
  , ''Gun
  , ''Bullet
  , ''Roid
  , ''Boom
  , ''Time
  , ''ChaseCam
  , ''Stars
  , ''Camera3
  , ''Env3
  , ''Camera
  ]

-- * Vector boundary

-- | Box3D vectors to linear, at the rendering/steering boundary.
v3 :: Vec3 -> V3 Float
v3 (Vec3 x y z) = V3 x y z

-- | And back, for physics writes.
vec3 :: V3 Float -> Vec3
vec3 (V3 x y z) = Vec3 x y z

-- | Box3D quaternions to linear (which carries w first).
q3 :: Quat -> Quaternion Float
q3 (Quat (Vec3 x y z) w) = Quaternion w (V3 x y z)

-- * Ship hulls

{- | A dart-shaped convex hull: a nose point ahead of a hexagonal tail
ring — wingtips wide, deck low. Returns the hull points (for the
physics 'GeoHull') and the render 'Solid'.
-}
dart :: Float -> Float -> Float -> Float -> Float -> ([V3 Float], Solid)
dart fore aft halfSpan top bot = (nose : ring, facesSolid (ring : sides))
  where
    nose = V3 0 (0.5 * (top + bot)) fore
    ring =
      [ V3 halfSpan 0 (-aft)
      , V3 (0.32 * halfSpan) top (-aft)
      , V3 (-0.32 * halfSpan) top (-aft)
      , V3 (-halfSpan) 0 (-aft)
      , V3 (-0.32 * halfSpan) bot (-aft)
      , V3 (0.32 * halfSpan) bot (-aft)
      ]
    sides = [[nose, a, b] | (a, b) <- zip ring (drop 1 ring <> take 1 ring)]

-- | Long, slim, fast-looking.
courierHull :: ([V3 Float], Solid)
courierHull = dart 1.7 1.1 0.9 0.3 (-0.22)

-- | Short, wide, angry-looking.
eagleHull :: ([V3 Float], Solid)
eagleHull = dart 0.9 0.55 0.75 0.2 (-0.15)

-- * Asteroids

-- | Unit icosahedron vertices.
icoVerts :: [V3 Float]
icoVerts =
  [ normalize u
  | a <- [-1, 1]
  , b <- [-1, 1]
  , u <- [V3 0 a (b * phi), V3 a (b * phi) 0, V3 (b * phi) 0 a]
  ]
  where
    phi = (1 + sqrt 5) / 2

{- | Icosahedron faces as vertex indices: the triples of mutually
adjacent vertices (every such triple is a face).
-}
icoFaceIx :: [[Int]]
icoFaceIx =
  [ [i, j, k]
  | (i, u) <- ixed
  , (j, v) <- ixed
  , (k, w) <- ixed
  , i < j
  , j < k
  , near u v && near v w && near u w
  ]
  where
    ixed = zip [0 :: Int ..] icoVerts
    near a b = norm (a - b) < 1.2

-- * Scene setup

arenaR :: Float
arenaR = 208

bulletSpeed :: Float
bulletSpeed = 30

initialize :: SystemT World IO ()
initialize = do
  set global (Gravity vec3Zero)
  set global env3{e3FogNear = 60, e3FogFar = 480, e3FogFloor = 0.25}
  stars <- liftIO . replicateM 240 $ do
    dir <- randomUnit
    b <- randomRIO (0.3, 1)
    s <- randomRIO (0.7, 1.8)
    pure (dir, b, s)
  set global (Stars stars)
  replicateM_ 128 spawnAsteroid
  spawnShip Courier white courierHull (V3 0 0 0) (V3 0 0 7)
  spawnShip Eagle red eagleHull (V3 3 1 (-10)) (V3 0 0 8)
  spawnShip Eagle orange eagleHull (V3 (-3) (-1) (-10)) (V3 0 0 8)

randomUnit :: IO (V3 Float)
randomUnit = do
  x <- randomRIO (-1, 1)
  y <- randomRIO (-1, 1)
  z <- randomRIO (-1, 1)
  let
    u = V3 x y z
    q = norm u
  if q < 0.08 || q > 1 then randomUnit else pure (u ^/ q)

{- | A tumbling rock: a radially jittered icosahedron, the same points
feeding the physics hull and the render solid. Carries category bit 2
('rockFilter') so the pilots' queries see only rocks.
-}
spawnAsteroid :: SystemT World IO ()
spawnAsteroid = do
  dir <- liftIO randomUnit
  d <- liftIO (randomRIO (48, 288))
  r <- liftIO (randomRIO (3.6, 9.6))
  spin <- liftIO randomUnit
  rate <- liftIO (randomRIO (0.05, 0.4))
  warm <- liftIO (randomRIO (0, 1))
  jit <- liftIO (mapM (const (randomRIO (0.72, 1.18))) icoVerts)
  let
    vs = zipWith (\k u -> (k * r) *^ u) jit icoVerts
    faces = [map (vs !!) ixs | ixs <- icoFaceIx]
    tint = mixColors warm (1 - warm) (makeColor 0.62 0.47 0.32 1) (greyN 0.55)
  e <-
    newEntity
      ( DynamicBody
      , Position (vec3 (dir ^* d))
      , AngularVelocity (vec3 (rate *^ spin))
      , LookSolid tint (facesSolid faces)
      , Roid r
      )
  newEntity_ (Shape e (GeoHull (VS.fromList (map vec3 vs))), Density 4, CollisionFilter rockFilter)

spawnShip :: Pilot -> Color -> ([V3 Float], Solid) -> V3 Float -> V3 Float -> SystemT World IO ()
spawnShip pilotKind tint (pts, solid) pos vel = do
  e <-
    newEntity
      ( DynamicBody
      , Position (vec3 pos)
      , Velocity (vec3 vel)
      , (LinearDamping 0.6, AngularDamping 1.1)
      , LookSolid tint solid
      , pilotKind
      , Gun 1
      )
  newEntity_ (Shape e (GeoHull (VS.fromList (map vec3 pts))), Density 1, Elasticity 0.35, CollisionFilter craftFilter)

fireBullet :: V3 Float -> V3 Float -> SystemT World IO ()
fireBullet pos vel = do
  b <-
    newEntity
      ( DynamicBody
      , BulletBody True
      , Position (vec3 pos)
      , Velocity (vec3 vel)
      , Bullet 2.2
      , LookDot yellow 0.07
      )
  newEntity_ (Shape b (GeoSphere vec3Zero 0.07), Density 20, Elasticity 0.4, CollisionFilter craftFilter)

-- * Autopilots

{- | The PD attitude-and-throttle controller every ship shares: torque
toward the desired direction, damped by the current spin; thrust
along the nose, throttled down when pointing the wrong way. Gains
scale with body mass so both hulls fly alike. Returns the nose
direction and its alignment with the request, for fire control.
-}
pilot :: Entity -> V3 Float -> Float -> SystemT World IO (V3 Float, Float)
pilot e desired accel = do
  (Rotation quat, AngularVelocity wv, BodyMass m) <- get e
  let
    fwd = Linear.rotate (q3 quat) (V3 0 0 1)
    d = normalize desired
    a = dot fwd d
    axis
      | a < -0.98 = V3 0 1 0
      | otherwise = cross fwd d
    torque = m *^ (4 *^ axis - 2.4 *^ v3 wv)
    push = (m * accel * (0.3 + 0.7 * max 0 a)) *^ fwd
  set e (Torque (vec3 torque), Force (vec3 push))
  pure (fwd, a)

{- | Rock shapes carry category bit 2 so queries can single them out.
Ships and bullets get category 1 explicitly — Box3D's default is all
bits set, which would match any query mask.
-}
rockFilter, craftFilter :: Filter
rockFilter = Filter 2 maxBound 0
craftFilter = Filter 1 maxBound 0

-- | Query filter matching only rocks.
rocksOnly :: Filter
rocksOnly = Filter 1 2 0

-- | Clear line of sight (no rock between the two points)?
lineOfSight :: V3 Float -> V3 Float -> SystemT World IO Bool
lineOfSight a b = do
  hit <- segmentQuery (vec3 a) (vec3 b) rocksOnly
  pure (null hit)

{- | Steer around what is actually in the flight path: a ray along the
velocity finds the rock that matters (if any) and deflects across it,
harder as it nears; a small 'pointQuery' bubble shoves off rocks
already alongside. Nothing pushes on a clear path, so pilots cruise
through the field instead of skirting it.
-}
avoidRocks :: V3 Float -> V3 Float -> V3 Float -> SystemT World IO (V3 Float)
avoidRocks p v fallback = do
  let
    speed = norm v
    dir = if speed > 1 then v ^/ speed else fallback
    lookahead = 18 + 2.2 * speed
  hit <- segmentQuery (vec3 p) (vec3 (p + dir ^* lookahead)) rocksOnly
  let dodge = case hit of
        Nothing -> V3 0 0 0
        Just RayHit{normal = n', fraction = f} ->
          let
            n = v3 n'
            lat = n - dot n dir *^ dir
            latDir = if quadrance lat < 0.002 then perpTo dir else normalize lat
          in
            (2.5 * (1 - f)) *^ latDir + (0.8 * (1 - f)) *^ n
  nears <- pointQuery (vec3 p) 16 rocksOnly
  bubble <- mapM push nears
  pure (dodge + sum bubble)
  where
    push e = do
      rock <- get e
      pure $ case rock of
        Nothing -> V3 0 0 0
        Just (Roid r, Position c) ->
          let
            off = p - v3 c
            gap = max 0.1 (norm off - r - 3)
          in
            if gap < 6 then
              (3 / (gap * gap + 0.3)) *^ normalize off
            else
              V3 0 0 0

-- | A pull back toward the origin past the arena radius.
arenaPull :: V3 Float -> V3 Float
arenaPull p
  | d > arenaR = ((d - arenaR) * 0.25) *^ negate (p ^/ d)
  | otherwise = V3 0 0 0
  where
    d = norm p

-- | Something perpendicular to the direction, for jinking sideways.
perpTo :: V3 Float -> V3 Float
perpTo u
  | norm s < 0.1 = V3 1 0 0
  | otherwise = normalize s
  where
    s = cross u (V3 0 1 0)

{- | Run from the Eagles' midpoint, weave on two incommensurate sine
waves, dodge what the raycast flags, stay in the arena — and pop the
tail gun at any Eagle sitting in the rear cone with a clear line.
-}
flyCourier :: Float -> [(Entity, V3 Float, V3 Float)] -> Entity -> V3 Float -> V3 Float -> SystemT World IO ()
flyCourier t eagles e p v = do
  let
    threat = sum [ep | (_, ep, _) <- eagles] ^/ fromIntegral (max 1 (length eagles))
    away = case eagles of
      [] -> V3 0 0 1
      _ -> normalize (p - threat)
    jink = (0.55 * sin (0.9 * t)) *^ perpTo away + (0.4 * sin (0.6 * t + 1)) *^ V3 0 1 0
  avoid <- avoidRocks p v away
  (fwd, _) <- pilot e (away + jink + avoid + arenaPull p) 11
  Gun cool <- get e
  let
    rear = negate fwd
    muzzle = p + rear ^* 2
    lined = [ep | (_, ep, _) <- eagles, norm (ep - p) < 20, dot (normalize (ep - p)) rear > 0.92]
  case lined of
    (target : _) | cool <= 0 -> do
      clear <- lineOfSight muzzle target
      when clear $ do
        set e (Gun 0.5)
        fireBullet muzzle (v + bulletSpeed *^ rear)
    _ -> pure ()

{- | Lead-pursue the Courier (aim where it will be at slug flight time),
dodge what the raycast flags, stay in the arena — and fire when the
nose is on target, in range, and no rock blocks the shot.
-}
flyEagle :: Entity -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> SystemT World IO ()
flyEagle e p v cp cv = do
  let
    sep = cp - p
    dist = norm sep
    lead = cp + cv ^* (dist / bulletSpeed)
    pursue = normalize (lead - p)
  avoid <- avoidRocks p v pursue
  (fwd, _) <- pilot e (pursue + avoid + arenaPull p) 13
  Gun cool <- get e
  let muzzle = p + fwd ^* 1.4
  when (cool <= 0 && dist < 26 && dot fwd (normalize sep) > 0.94) $ do
    clear <- lineOfSight muzzle cp
    when clear $ do
      set e (Gun 0.4)
      fireBullet muzzle (v + bulletSpeed *^ fwd)

-- * Loop

step :: Float -> SystemT World IO ()
step dT' = do
  let dT = min dT' (1 / 30)
  Time t <- get global
  set global (Time (t + dT))
  cmap $ \(Gun c) -> Gun (max 0 (c - dT))
  cmapM_ $ \(Bullet ttl, e :: Entity) ->
    if ttl <= dT then
      destroy e (Proxy @(Body, Bullet, Look))
    else
      set e (Bullet (ttl - dT))
  ships <- cfold (\acc (kind :: Pilot, Position p, Velocity v, e :: Entity) -> (kind, e, v3 p, v3 v) : acc) []
  let
    eagles = [(e, p, v) | (Eagle, e, p, v) <- ships]
    couriers = [(e, p, v) | (Courier, e, p, v) <- ships]
  forM_ couriers $ \(e, p, v) -> flyCourier t eagles e p v
  forM_ eagles $ \(e, p, v) ->
    forM_ (take 1 couriers) $ \(_, cp, cv) -> flyEagle e p v cp cv
  stepPhysics dT
  detonations dT
  forM_ (take 1 couriers) $ \(e, _, _) -> chaseCamera e

{- | Turn the step's 'Impacts' into pyrotechnics: slugs expend
themselves in a flash sized by the approach speed (the momentum was
already delivered by the engine), and any other hard impact throws a
smaller dust burst. Then age out the flashes.
-}
detonations :: Float -> SystemT World IO ()
detonations dT = do
  Impacts hits <- get global
  forM_ hits $ \hit -> do
    spent <- fmap or . forM [hit.bodyA, hit.bodyB] $ \b -> do
      slug <- get b
      case slug of
        Just (Bullet _) -> do
          destroy b (Proxy @(Body, Bullet, Look))
          pure True
        Nothing -> pure False
    let
      speed = hit.speed
      mag
        | spent = 0.5 + speed * 0.04
        | otherwise = 0.25 + speed * 0.02
    when (spent || speed > 6) $
      newEntity_ (Boom (v3 hit.point) mag 0)
  cmapM_ $ \(Boom pos mag age, e :: Entity) ->
    if age > boomLife then
      destroy e (Proxy @Boom)
    else
      set e (Boom pos mag (age + dT))

-- | Trail the Courier: smoothed eye behind its motion, looking ahead.
chaseCamera :: Entity -> SystemT World IO ()
chaseCamera e = do
  (Position p, Velocity vv, Rotation quat) <- get e
  let
    cp = v3 p
    cv = v3 vv
    fwd = Linear.rotate (q3 quat) (V3 0 0 1)
    dir = if norm cv > 2 then normalize cv else fwd
    wantEye = cp - dir ^* 7.5 + V3 0 2.4 0
    wantTgt = cp + dir ^* 3
  ChaseCam eye0 tgt0 <- get global
  let
    eye = Linear.lerp 0.06 eye0 wantEye
    tgt = Linear.lerp 0.18 tgt0 wantTgt
  set global (ChaseCam eye tgt)
  set global (Camera3 eye tgt (V3 0 1 0) 440)

-- | Contacts beyond this distance fall off the scanner.
radarRange :: Float
radarRange = 100

{- | The classic scanner: an ellipse showing the ship's horizontal
plane (oriented like the chase view: up on the scanner is ahead),
each contact a lollipop — a dash where it sits on the plane, a
vertical stick for its height above or below, a blip at the tip.
-}
drawRadar :: Camera3 -> SystemT World IO Picture
drawRadar cam = do
  couriers <- cfold (\acc (kind :: Pilot, Position p) -> [v3 p | Courier <- [kind]] <> acc) []
  case couriers of
    [] -> pure Blank
    (c : _) -> do
      blips <-
        cfold
          (\acc (look :: Look, Position p) -> case look of LookSolid tint _ -> (tint, v3 p) : acc; _ -> acc)
          []
      pure (radarPicture cam c blips)

radarPicture :: Camera3 -> V3 Float -> [(Color, V3 Float)] -> Picture
radarPicture cam c blips = Translate 0 (-250) (Pictures (frame <> marks))
  where
    up = V3 0 1 0
    flat = (c3Target cam - c3Eye cam) * V3 1 0 1
    ahead = if quadrance flat < 1e-4 then V3 0 0 1 else normalize flat
    right = normalize (cross ahead up)
    rx = 130
    squash = 0.42
    s = rx / radarRange
    frame =
      [ Color (greyN 0.35) (Scale 1 squash (circle rx))
      , Color (greyN 0.2) (Line [(-rx, 0), (rx, 0)])
      , Color (greyN 0.2) (Line [(0, -rx * squash), (0, rx * squash)])
      , Color white (circleSolid 3)
      ]
    marks =
      [ Pictures
          [ Color (dim tint) (Line [(px, py), (px, py + hp)])
          , Color (dim tint) (Translate px py (rectangleSolid 6 1.2))
          , Color tint (Translate px (py + hp) (circleSolid 2.5))
          ]
      | (tint, p) <- blips
      , let rel = p - c
      , quadrance rel > 1
      , quadrance rel < radarRange * radarRange
      , let
          px = dot rel right * s
          py = dot rel ahead * s * squash
          hp = dot rel up * s * 0.5
      ]

boomLife :: Float
boomLife = 0.45

{- | An expanding two-tone fireball: a hot core inside a reddening,
fading shell, both sphere impostors.
-}
drawBoom :: Scene3 -> Boom -> [Piece]
drawBoom sc (Boom pos mag age) =
  sphere3 sc shell (mag * (0.5 + 3 * t)) pos
    <> sphere3 sc core (mag * (0.3 + 1.2 * t)) pos
  where
    t = min 1 (age / boomLife)
    shell = makeColor 1 (0.6 - 0.45 * t) 0.1 (0.75 * (1 - t))
    core = makeColor 1 1 (0.8 - 0.5 * t) (1 - 0.6 * t)

drawLook :: Scene3 -> Vec3 -> Quat -> Look -> [Piece]
drawLook sc p q = \case
  LookSolid tint solid -> solid3 sc tint solid (v3 p) (q3 q)
  LookDot tint r -> sphere3 sc tint r (v3 p)

draw :: SystemT World IO Picture
draw = do
  cam <- get global
  env <- get global
  Stars stars <- get global
  let
    sc = scene3 cam env
    starfield =
      [ (1e6, Translate px py (Color (greyN b) (circleSolid s)))
      | (dir, b, s) <- stars
      , let vp@(V3 _ _ z) = viewPoint sc (c3Eye cam + dir ^* 400)
      , z > 1
      , let (px, py) = projectPoint sc vp
      ]
  pieces <- cfold (\acc (look :: Look, Position p, Rotation q) -> drawLook sc p q look <> acc) []
  booms <- cfold (\acc (b :: Boom) -> drawBoom sc b <> acc) []
  radar <- drawRadar cam
  pure (Pictures [assemble (starfield <> pieces <> booms), radar])

handle :: Event -> SystemT World IO ()
handle (EventKey (MouseButton LeftButton) Down _ _) = do
  dir <- liftIO randomUnit
  spawnShip Eagle (dim red) eagleHull (arenaR *^ dir) (negate (8 *^ dir))
handle _ = pure ()

{- | Headless verification and recording: warm the simulation up at a
fixed 60 Hz, then export frames (every @stride@th simulated frame)
straight to PNGs via gloss-export — exact pixels, no window, no
compositor in the way. Prints the explosion pieces in view per frame.
Stride 1 films every frame, ready for encoding into a 60 fps video.

The fixed timestep plus the seeded generator (main prints the seed)
make film runs fully deterministic: the same seed and parameters
reproduce the encounter bit for bit.
-}
film :: Float -> Int -> Int -> String -> SystemT World IO ()
film warm count stride prefix = do
  replicateM_ (round (warm * 60)) (step (1 / 60))
  forM_ [1 .. count] $ \i -> do
    replicateM_ (max 1 stride) (step (1 / 60))
    pic <- draw
    cam <- get global
    env <- get global
    let sc = scene3 cam env
    flashes <- cfold (\acc (b :: Boom) -> drawBoom sc b <> acc) []
    let name = prefix <> pad i <> ".png"
    liftIO $ do
      exportPictureToPNG (960, 720) black name pic
      putStrLn (name <> " boomPiecesInView=" <> show (length flashes))
  where
    pad i = let s = show i in replicate (4 - length s) '0' <> s

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("film" : warm : count : stride : prefix : rest) -> do
      seed <- case rest of
        [s] -> pure (read s)
        _ -> randomRIO (0, 999999 :: Int)
      putStrLn ("seed " <> show seed)
      setStdGen (mkStdGen seed)
      w <- initWorld
      runSystem (initialize >> film (read warm) (read count) (read stride) prefix) w
    _ -> do
      w <- initWorld
      runSystem (initialize >> play disp black 60 draw handle step) w
  where
    disp = InWindow "elitelite" (960, 720) (10, 10)
