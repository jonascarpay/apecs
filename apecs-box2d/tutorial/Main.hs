{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| Arena tag: the game built step by step in tutorial/README.md.

Drive a puck through a walled arena and tag the totem with a ray
before it loses patience and relocates. The @shots@ mode renders the
tutorial screenshots headlessly with gloss-export, one staged world
per picture:

@
apecs-box2d-tutorial -- shots [directory]
@
-}
module Main (main) where

import Apecs
import Apecs.Gloss
import Control.Monad (forM_, replicateM_, unless, when)
import Data.Char (toLower)
import Data.Set qualified as Set
import Graphics.Gloss.Export.PNG (exportPictureToPNG)
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Linear (V2 (..))
import System.Environment (getArgs)
import System.Random (mkStdGen, randomRIO, setStdGen)

import Apecs.Box2D
import Box2D.MathTypes (vec2Add, vec2Length, vec2MulAdd, vec2Normalize, vec2Scale, vec2Sub)

-- * Components

-- | Local-space picture, placed by the entity's Box2D body transform.
newtype Look = Look Picture

instance Component Look where
  type Storage Look = Map Look

-- | The player's puck (at most one).
data Player = Player

instance Component Player where
  type Storage Player = Unique Player

-- | The tag totem (at most one).
data Target = Target

instance Component Target where
  type Storage Target = Unique Target

-- | A tag-ray flash: endpoints, whether it tagged, seconds left.
data Beam = Beam WVec WVec Bool Float

instance Component Beam where
  type Storage Beam = Map Beam

-- | An expanding ring marking a hard impact: center and age.
data Ding = Ding WVec Float

instance Component Ding where
  type Storage Ding = Map Ding

-- | The movement keys currently held.
newtype Keys = Keys (Set.Set Key)

instance Semigroup Keys where
  Keys a <> Keys b = Keys (a <> b)

instance Monoid Keys where
  mempty = Keys mempty

instance Component Keys where
  type Storage Keys = Global Keys

-- | The mouse cursor, in world coordinates.
newtype Aim = Aim WVec

instance Semigroup Aim where
  _ <> b = b

instance Monoid Aim where
  mempty = Aim vec2Zero

instance Component Aim where
  type Storage Aim = Global Aim

-- | Totems tagged so far.
newtype Score = Score Int

instance Semigroup Score where
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty = Score 0

instance Component Score where
  type Storage Score = Global Score

-- | Seconds until the totem relocates on its own.
newtype Patience = Patience Float

instance Semigroup Patience where
  _ <> b = b

instance Monoid Patience where
  mempty = Patience 0

instance Component Patience where
  type Storage Patience = Global Patience

makeWorld
  "World"
  [ ''Physics
  , ''Camera
  , ''Look
  , ''Player
  , ''Target
  , ''Beam
  , ''Ding
  , ''Keys
  , ''Aim
  , ''Score
  , ''Patience
  ]

-- * Tuning

arenaHalf, playerRadius, targetRadius, thrust, tagRange, patienceLimit, beamFade, dingFade :: Float
arenaHalf = 10
playerRadius = 0.4
targetRadius = 0.5
thrust = 30
tagRange = 40
patienceLimit = 5
beamFade = 0.25
dingFade = 0.4

background :: Color
background = greyN 0.12

-- * Setup

initialize :: SystemT World IO ()
initialize = do
  makeArena
  makePlayer
  makeTarget

-- | Camera, zero gravity, the border and the random inner walls.
makeArena :: SystemT World IO ()
makeArena = do
  set global (Camera 0 32, Gravity vec2Zero)
  makeBorder
  replicateM_ 9 makeObstacle

-- | One static body fenced in by four segment shapes.
makeBorder :: SystemT World IO ()
makeBorder = do
  let
    h = arenaHalf
    corners = [(-h, -h), (h, -h), (h, h), (-h, h)]
  border <- newEntity (StaticBody, Look (Color white (lineLoop corners)))
  forM_ (zip corners (drop 1 corners <> take 1 corners)) $ \((x1, y1), (x2, y2)) ->
    newEntity_ (Shape border (GeoSegment (Vec2 x1 y1) (Vec2 x2 y2)))

{- | A static wall of random shape, size, position and rotation.

Placement allows for the wall's reach — the farthest any part sticks
out from the body origin — so nothing covers the player spawn in the
middle or pokes through the border.
-}
makeObstacle :: SystemT World IO ()
makeObstacle = do
  kind <- liftIO (randomRIO (0 :: Int, 2))
  (geo, reach, pic) <- liftIO $ case kind of
    0 -> do
      hw <- randomRIO (0.4, 1.2)
      hh <- randomRIO (0.4, 1.2)
      pure (GeoBox hw hh, sqrt (hw * hw + hh * hh), rectangleSolid (2 * hw) (2 * hh))
    1 -> do
      r <- randomRIO (0.5, 1)
      pure (GeoCircle vec2Zero r, r, circleSolid r)
    _ -> do
      l <- randomRIO (1, 2.5)
      r <- randomRIO (0.25, 0.4)
      pure
        ( GeoCapsule (Vec2 (-l) 0) (Vec2 l 0) r
        , l + r
        , Pictures
            [ Translate (-l) 0 (circleSolid r)
            , Translate l 0 (circleSolid r)
            , rectangleSolid (2 * l) (2 * r)
            ]
        )
  theta <- liftIO (randomRIO (0, pi))
  p <- liftIO (randomBetween (2 + reach) (arenaHalf - 0.5 - reach))
  wall <- newEntity (StaticBody, Position p, Angle theta, Look (Color (greyN 0.5) pic))
  newEntity_ (Shape wall geo)

-- | A random point whose distance from the center is between the bounds.
randomBetween :: Float -> Float -> IO Vec2
randomBetween rMin rMax = do
  x <- randomRIO (-rMax, rMax)
  y <- randomRIO (-rMax, rMax)
  let v = Vec2 x y
  if vec2Length v < rMin || vec2Length v > rMax then
    randomBetween rMin rMax
  else
    pure v

-- | A dynamic puck at the center; damping stands in for floor friction.
makePlayer :: SystemT World IO ()
makePlayer = do
  player <-
    newEntity
      (DynamicBody, LinearDamping 5, Player, Look (Color azure (circleSolid playerRadius)))
  newEntity_ (Shape player (GeoCircle vec2Zero playerRadius), Elasticity 0.3)

-- | The static totem the player is trying to tag.
makeTarget :: SystemT World IO ()
makeTarget = do
  let look = Color orange (Pictures [circleSolid (targetRadius * 0.4), ThickCircle targetRadius 0.1])
  target <- newEntity (StaticBody, Target, Look look)
  newEntity_ (Shape target (GeoCircle vec2Zero targetRadius))
  relocateTarget

-- | Teleport the totem to a free spot and reset its patience.
relocateTarget :: SystemT World IO ()
relocateTarget = do
  set global (Patience patienceLimit)
  cmapM_ $ \(Target, target) -> do
    p <- freeSpot
    set target (Position p)

-- | A random spot where a totem-sized circle overlaps nothing.
freeSpot :: SystemT World IO WVec
freeSpot = do
  p <- liftIO (randomBetween 2 (arenaHalf - 1))
  occupied <- overlapQuery (GeoCircle p (targetRadius + 0.1)) everything
  if null occupied then pure p else freeSpot

-- * Input

handle :: Event -> SystemT World IO ()
handle event = case event of
  EventKey (MouseButton LeftButton) Down _ screenPos -> do
    updateAim screenPos
    fireTag
  EventMotion screenPos -> updateAim screenPos
  EventKey key Down _ _ -> modify global (\(Keys held) -> Keys (Set.insert (unshift key) held))
  EventKey key Up _ _ -> modify global (\(Keys held) -> Keys (Set.delete (unshift key) held))
  _ -> pure ()

{- | Normalize a key's case.

Gloss reports character keys with Shift applied: a 'w' pressed plain
but released with Shift held ('W') would never leave the held set — a
stuck key.
-}
unshift :: Key -> Key
unshift (Char c) = Char (toLower c)
unshift key = key

updateAim :: (Float, Float) -> SystemT World IO ()
updateAim screenPos = do
  camera <- get global
  let V2 x y = windowToWorld camera screenPos
  set global (Aim (Vec2 x y))

{- | Cast a tag ray from the player towards the aim point.

The segment starts inside the player's own shape, which 'segmentQuery'
ignores, so the player never tags itself; the first thing along the
ray decides whether it's a tag.
-}
fireTag :: SystemT World IO ()
fireTag = cmapM_ $ \(Player, Position from) -> do
  Aim at <- get global
  let dir = vec2Sub at from
  when (vec2Length dir > 0.001) $ do
    let far = vec2MulAdd from tagRange (vec2Normalize dir)
    hit <- segmentQuery from far everything
    case hit of
      Nothing -> newEntity_ (Beam from far False beamFade)
      Just RayHit{body, point} -> do
        tagged <- exists body (Proxy @Target)
        newEntity_ (Beam from point tagged beamFade)
        when tagged $ do
          modify global (\(Score n) -> Score (n + 1))
          relocateTarget

-- * Stepping

step :: Float -> SystemT World IO ()
step rawDt = do
  let dT = min rawDt (1 / 30)
  drivePlayer
  stepPhysics dT
  ringImpacts
  tickTimers dT

-- | Sum the held movement keys into a thrust force on the player.
drivePlayer :: SystemT World IO ()
drivePlayer = do
  Keys held <- get global
  let
    bindings =
      [ ([Char 'w', SpecialKey KeyUp], Vec2 0 1)
      , ([Char 's', SpecialKey KeyDown], Vec2 0 (-1))
      , ([Char 'a', SpecialKey KeyLeft], Vec2 (-1) 0)
      , ([Char 'd', SpecialKey KeyRight], Vec2 1 0)
      ]
    dir = foldl vec2Add vec2Zero [v | (keys, v) <- bindings, any (`Set.member` held) keys]
  when (vec2Length dir > 0) $
    cmap $
      \Player -> Force (vec2Scale thrust (vec2Normalize dir))

-- | Ring every impact hard enough to make a hit event.
ringImpacts :: SystemT World IO ()
ringImpacts = do
  Impacts hits <- get global
  forM_ hits $ \Impact{point} -> newEntity_ (Ding point 0)

-- | Fade the flashes and relocate the totem when patience runs out.
tickTimers :: Float -> SystemT World IO ()
tickTimers dT = do
  cmap $ \(Beam a b tagged t) -> if t <= dT then Nothing else Just (Beam a b tagged (t - dT))
  cmap $ \(Ding p age) -> if age > dingFade then Nothing else Just (Ding p (age + dT))
  Patience t <- get global
  if t <= dT then relocateTarget else set global (Patience (t - dT))

-- * Drawing

draw :: SystemT World IO Picture
draw = do
  bodies <- drawBodies
  beams <- foldDraw drawBeam
  dings <- foldDraw drawDing
  aim <- drawAim
  hud <- drawHud
  pure (Pictures [bodies, beams, dings, aim, hud])

-- | Read each body's transform back from Box2D and place its picture.
drawBodies :: SystemT World IO Picture
drawBodies = foldDraw $ \(Position (Vec2 x y), Angle theta, Look pic) ->
  Translate x y (Rotate (negate (radToDeg theta)) pic)

drawBeam :: Beam -> Picture
drawBeam (Beam (Vec2 ax ay) (Vec2 bx by) tagged t) =
  Color (withAlpha (t / beamFade) (if tagged then chartreuse else red)) (Line [(ax, ay), (bx, by)])

drawDing :: Ding -> Picture
drawDing (Ding (Vec2 x y) age) =
  Color (withAlpha (1 - age / dingFade) orange) (Translate x y (ThickCircle (age * 4) 0.06))

-- | An aim line and crosshair, only while a player is around.
drawAim :: SystemT World IO Picture
drawAim = do
  Aim (Vec2 ax ay) <- get global
  let cross = Translate ax ay (Pictures [Line [(-0.25, 0), (0.25, 0)], Line [(0, -0.25), (0, 0.25)]])
  foldDraw $ \(Player, Position (Vec2 px py)) ->
    Color (withAlpha 0.35 azure) (Pictures [Line [(px, py), (ax, ay)], cross])

drawHud :: SystemT World IO Picture
drawHud = do
  Score n <- get global
  pure $
    Translate (-arenaHalf) (arenaHalf + 0.4) $
      Scale 0.008 0.008 (Color white (Text ("Tags: " <> show n)))

-- * Wiring

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("shots" : rest) -> shots (case rest of dir : _ -> dir; [] -> ".")
    _ -> do
      w <- initWorld
      runSystem (initialize >> play disp background 60 draw handle step) w
  where
    disp = InWindow "arena tag" (720, 720) (10, 10)

{- | Render the tutorial screenshots.

Each stage seeds its own generator, so a single screenshot can be
regenerated in isolation. A stage whose scripted action doesn't come
off (a missed tag, no impact) fails the run instead of exporting a
picture that contradicts the prose — pick another seed.
-}
shots :: FilePath -> IO ()
shots dir = do
  snap 2026 (dir <> "/1-arena.png") drawBodies $
    makeArena
  snap 2026 (dir <> "/2-drive.png") drawBodies $ do
    makeArena
    makePlayer
    holdKeys [Char 'd', Char 'w']
    replicateM_ 66 (drivePlayer >> stepPhysics (1 / 60))
  snap 2026 (dir <> "/3-tag.png") draw $ do
    initialize
    holdKeys [Char 'a']
    replicateM_ 45 (step (1 / 60))
    holdKeys []
    aimAtTarget
    fireTag
    Score n <- get global
    when (n == 0) (liftIO (fail "3-tag: the staged tag missed"))
  snap 2026 (dir <> "/4-bonk.png") draw $ do
    initialize
    holdKeys [Char 'd']
    stepUntilDing (600 :: Int)
    replicateM_ 8 (step (1 / 60))
    aimAtTarget
  where
    holdKeys keys = set global (Keys (Set.fromList keys))
    aimAtTarget = cmapM_ $ \(Target, Position p) -> set global (Aim p)
    stepUntilDing 0 = liftIO (fail "4-bonk: nothing slammed a wall in time")
    stepUntilDing n = do
      step (1 / 60)
      rung <- cfold (\_ (_ :: Ding) -> True) False
      unless rung (stepUntilDing (n - 1 :: Int))

{- | Seed, build a fresh world, run a stage, export one frame.

The frame is drawn the way 'play' would show it. The engine world is
destroyed afterwards: Box2D keeps worlds in a fixed-size registry, so
a process that makes many must destroy them too.
-}
snap :: Int -> FilePath -> SystemT World IO Picture -> SystemT World IO () -> IO ()
snap seed path drawStage stage = do
  setStdGen (mkStdGen seed)
  w <- initWorld
  runSystem go w
  where
    go = do
      stage
      camera <- get global
      pic <- drawStage
      liftIO (exportPictureToPNG (720, 720) background path (cameraTransform camera pic))
      destroyPhysics
