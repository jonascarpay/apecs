{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

import Apecs
import Data.Proxy
import Control.Monad
import Linear
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data Target = Target deriving Show
instance Component Target where type Storage Target = Map Target

data Bullet = Bullet deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

newtype Score = Score Int deriving Show
instance Monoid Score where mempty = Score 0
instance Component Score where type Storage Score = Global Score

newtype Time = Time Double deriving Show
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

data Particle = Particle Color Double deriving Show
instance Component Particle where type Storage Particle = Map Particle

makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle]

type System' a = System World a
type Movable = (Position, Velocity)

playerSpeed = 170
playerPos = V2 0 (-120)
scorePos  = V2 xmin (-170)
bulletSpeed = 500
enemySpeed  = 80
xmin = -100
xmax = 100

spawnParticles n pos color dvx dvy = replicateM_ n $ do
  vx <- liftIO $ randomRIO dvx
  vy <- liftIO $ randomRIO dvy
  t  <- liftIO $ randomRIO (0.02,0.3)
  newEntity (Particle color t, Position pos, Velocity (V2 vx vy))

initialize :: System' ()
initialize = void $ newEntity (Player, Position playerPos, Velocity 0)

step :: Double -> System' ()
step dT = do
  incrTime
  stepPosition
  clampPlayer
  clearTargets
  clearBullets
  stepParticles
  handleCollisions
  triggerEvery 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
  triggerEvery 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))
  where
    incrTime = modify 0 $ \(Time t) -> Time (t+dT)
    stepPosition = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)
    clampPlayer = cmap $ \(Player, Position (V2 x y)) -> Position (V2 (min xmax . max xmin $ x) y)
    triggerEvery period phase sys = do
      Time t <- get global
      let t' = t + phase
      when (floor (t'/period) /= floor ((t'+dT)/period)) (void sys)
    clearBullets = cmapM_ $ \(Bullet, Position (V2 x y), ety) ->
      when (y > 170) $ do
        destroy ety (Proxy @(Bullet, Movable))
        modify global $ \(Score x) -> Score (x-40)
    clearTargets = cmap $ \c@(Target, Position (V2 x y), Velocity v) ->
      if x < xmin || x > xmax
         then Nothing
         else Just c
    handleCollisions =
      cmapM_ $ \t@(Target, Position pTarget, Velocity _, eTarget) ->
        cmapM_ $ \b@(Bullet, Position pBullet, Velocity _, eBullet) ->
          when (norm (pTarget - pBullet) < 10) $ do
            destroy eTarget (Proxy @(Target, Position, Velocity))
            destroy eBullet (Proxy @(Bullet, Position, Velocity))
            spawnParticles 15 pBullet white (-500,500) (200,-50)
            modify global $ \(Score x) -> Score (x + 100)
    stepParticles = cmapM_ $ \(Particle col t, ety) ->
      if t < 0
         then destroy ety (Proxy @(Particle, Movable))
         else set ety (Particle col (t-dT))

draw :: System' Picture
draw = do
  p <- toPic $ \(Player, Position p) -> color white  . translate' p . scale 10 20 $ triangle
  t <- toPic $ \(Target, Position p) -> color red    . translate' p . scale 10 10 $ diamond
  b <- toPic $ \(Bullet, Position p) -> color yellow . translate' p . scale 4  4   $ diamond
  s <- flip fmap (get global) $ \(Score s) -> color white . translate' scorePos . scale 0.1 0.1 . Text $ "Score: " ++ show s
  pt <- toPic $ \(Particle col _, Position p, Velocity (V2 vx vy)) -> color col . translate' p $ Line [(0,0),(realToFrac vx/10, realToFrac vy/10)]
  return $ mconcat [p,t,b,s,pt]
  where
    toPic f = mconcat . fmap f <$> getAll
    translate' (V2 x y) = translate (realToFrac x) (realToFrac y)
    triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
    diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x-playerSpeed) 0)

handleEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x+playerSpeed) 0)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x+playerSpeed) 0)

handleEvent (EventKey (SpecialKey KeyRight) Up   _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x-playerSpeed) 0)

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
  cmapM_ $ \(Player, Position p) -> do
    newEntity (Bullet, Position p, Velocity (V2 0 bulletSpeed))
    spawnParticles 7 p yellow (-80,80) (10,100)

handleEvent _ = return ()

playGloss :: w
          -> System w Picture
          -> (Event -> System w ())
          -> (Double -> System w ())
          -> IO ()
playGloss world drawSys eventSys stepSys =
  playIO
    window black fps ()
    (\_    -> runSystem drawSys world)
    (\e _  -> runSystem (eventSys e) world)
    (\dt _ -> runSystem (stepSys $ realToFrac dt) world)
  where
    window = InWindow "game" (220,360) (10,10)
    fps = 60

main = do
  w <- initWorld
  runSystem initialize w
  playGloss w draw handleEvent step
