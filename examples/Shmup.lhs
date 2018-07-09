test
>{-# LANGUAGE DataKinds             #-}
>{-# LANGUAGE FlexibleContexts      #-}
>{-# LANGUAGE MultiParamTypeClasses #-}
>{-# LANGUAGE ScopedTypeVariables   #-}
>{-# LANGUAGE TemplateHaskell       #-}
>{-# LANGUAGE TypeApplications      #-}
>{-# LANGUAGE TypeFamilies          #-}

more test

`aa`

>import           Apecs
>import           Control.Monad
>import           Data.Monoid
>import           Graphics.Gloss
>import           Graphics.Gloss.Interface.IO.Game
>import           Linear
>import           System.Random
>
>newtype Position = Position (V2 Double) deriving Show
>instance Component Position where type Storage Position = Map Position
>
>newtype Velocity = Velocity (V2 Double) deriving Show
>instance Component Velocity where type Storage Velocity = Map Velocity
>
>data Player = Player deriving Show
>instance Component Player where type Storage Player = Unique Player
>
>data Target = Target deriving Show
>instance Component Target where type Storage Target = Map Target
>
>data Bullet = Bullet deriving Show
>instance Component Bullet where type Storage Bullet = Map Bullet
>
>newtype Score = Score Int deriving Show
>instance Monoid Score where mempty = Score 0
>instance Component Score where type Storage Score = Global Score
>
>newtype Time = Time Double deriving Show
>instance Monoid Time where mempty = Time 0
>instance Component Time where type Storage Time = Global Time
>
>data Particle = Particle Color Double deriving Show
>instance Component Particle where type Storage Particle = Map Particle
>
>makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle]
>
>type System' a = System World a
>type Kinetic = (Position, Velocity)
>
>playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Double
>playerSpeed = 170
>bulletSpeed = 500
>enemySpeed  = 80
>xmin = -100
>xmax = 100
>
>hitBonus, missPenalty :: Int
>hitBonus = 100
>missPenalty = 40
>
>playerPos, scorePos :: V2 Double
>playerPos = V2 0 (-120)
>scorePos  = V2 xmin (-170)
>
>initialize :: System' ()
>initialize = void $ newEntity (Player, Position playerPos, Velocity 0)
>
>stepPosition :: Double -> System' ()
>stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)
>
>clampPlayer :: System' ()
>clampPlayer = cmap $ \(Player, Position (V2 x y))
>                   -> Position (V2 (min xmax . max xmin $ x) y)
>
>incrTime :: Double -> System' ()
>incrTime dT = modify global $ \(Time t) -> Time (t+dT)
>
>clearTargets :: System' ()
>clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
>  if x < xmin || x > xmax
>     then Nothing
>     else Just all
>
>stepParticles :: Double -> System' ()
>stepParticles dT = cmap $ \(Particle col t) ->
>  if t < 0
>     then Right $ Not @(Particle, Kinetic)
>     else Left  $ Particle col (t-dT)
>
>clearBullets :: System' ()
>clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
>  if y > 170
>     then Right $ (Not @(Bullet, Kinetic), Score (s-missPenalty))
>     else Left ()
>
>handleCollisions :: System' ()
>handleCollisions =
>  cmapM_ $ \(Target, Position posT, etyT) ->
>    cmapM_ $ \(Bullet, Position posB, etyB) ->
>      when (norm (posT - posB) < 10) $ do
>        destroy etyT (Proxy @(Target, Kinetic))
>        destroy etyB (Proxy @(Bullet, Kinetic))
>        spawnParticles 15 (Position posB) white (-500,500) (200,-50)
>        modify global $ \(Score x) -> Score (x + hitBonus)
>
>triggerEvery :: Double -> Double -> Double -> System' a -> System' ()
>triggerEvery dT period phase sys = do
>  Time t <- get global
>  let t' = t + phase
>      trigger = floor (t'/period) /= floor ((t'+dT)/period)
>  when trigger $ void sys
>
>spawnParticles :: Int -> Position -> Color -> (Double,Double) -> (Double,Double) -> System' ()
>spawnParticles n pos color dvx dvy = replicateM_ n $ do
>  vx <- liftIO $ randomRIO dvx
>  vy <- liftIO $ randomRIO dvy
>  t  <- liftIO $ randomRIO (0.02,0.3)
>  newEntity (Particle color t, pos, Velocity (V2 vx vy))
>
>step :: Double -> System' ()
>step dT = do
>  incrTime dT
>  stepPosition dT
>  clampPlayer
>  clearTargets
>  clearBullets
>  stepParticles dT
>  handleCollisions
>  triggerEvery dT 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
>  triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))
>
>handleEvent :: Event -> System' ()
>handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
>  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x-playerSpeed) 0)
>
>handleEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) =
>  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x+playerSpeed) 0)
>
>handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
>  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x+playerSpeed) 0)
>
>handleEvent (EventKey (SpecialKey KeyRight) Up   _ _) =
>  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x-playerSpeed) 0)
>
>handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
>  cmapM_ $ \(Player, pos) -> do
>    newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
>    spawnParticles 7 pos yellow (-80,80) (10,100)
>
>handleEvent _ = return ()
>
>drawComponents :: Get World c => (c -> Picture) -> System' Picture
>drawComponents f = cfold
>  (\pic (Position p, c) -> pic <> translate' p (f c))
>  mempty
>
>translate' :: V2 Double -> Picture -> Picture
>translate' (V2 x y) = translate (realToFrac x) (realToFrac y)
>
>triangle, diamond :: Picture
>triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
>diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]
>
>draw :: System' Picture
>draw = do
>  player  <- drawComponents $ \Player -> color white  . scale 10 20 $ triangle
>  targets <- drawComponents $ \Target -> color red    . scale 10 10 $ diamond
>  bullets <- drawComponents $ \Bullet -> color yellow . scale 4  4  $ diamond
>
>  particles <- drawComponents $
>    \(Particle col _, Velocity (V2 vx vy))
>    -> color col $ Line [(0,0),(realToFrac vx/10, realToFrac vy/10)]
>
>  Score s <- get global
>  let score = color white . translate' scorePos . scale 0.1 0.1 . Text $ "Score: " ++ show s
>
>  return $ player <> targets <> bullets <> score <> particles
>  where
>
>playGloss :: w
>          -> System w Picture
>          -> (Event -> System w ())
>          -> (Double -> System w ())
>          -> IO ()
>playGloss world drawSys eventSys stepSys =
>  playIO
>    window black fps ()
>    (\_    -> runSystem drawSys world)
>    (\e _  -> runSystem (eventSys e) world)
>    (\dt _ -> runSystem (stepSys $ realToFrac dt) world)
>  where
>    window = InWindow "game" (220,360) (10,10)
>    fps = 60
>
>main :: IO ()
>main = do
>  w <- initWorld
>  runSystem initialize w
>  playGloss w draw handleEvent step
