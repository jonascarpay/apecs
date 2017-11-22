{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Monad as M
import           Data.Proxy
import           SDL           (($=))
import qualified SDL
import           SDL.Vect
import           System.Random

import           Apecs
import qualified Apecs.Slice   as S

hres, vres :: Num a => a
hres = 1024
vres = 768

newtype Position = Position {getPos :: V2 Double} deriving (Show, Num)
instance Component Position where
  type Storage Position = Map Position

newtype Target = Target (V2 Double)
instance Component Target where
  type Storage Target = Map Target

data Selected = Selected
instance Flag Selected where flag = Selected
instance Component Selected where
  type Storage Selected = Set Selected

data MouseState = Dragging !(V2 Double) !(V2 Double) | Rest
instance Monoid MouseState where
  mempty = Rest
  mappend = undefined
instance Component MouseState where
  type Storage MouseState = Global MouseState

makeWorld "World" [''Position, ''Target, ''Selected, ''MouseState]

type System' a = System World a

game :: System' ()
game = do
  (window, renderer) <- initRenderer

  -- Add units
  replicateM_ 5000 $ do
    x <- liftIO$ randomRIO (100,hres/2)
    y <- liftIO$ randomRIO (100,vres-100)
    newEntity (Position (V2 x y))

  let loop = do
        shouldQuit <- handleEvents
        step
        render renderer
        unless shouldQuit loop

  loop

  cleanup (window, renderer)

render :: SDL.Renderer -> System' ()
render renderer = do
  liftIO$ SDL.rendererDrawColor renderer $= V4 0 0 0 255
  liftIO$ SDL.clear renderer

  cimapM_ $ \(e, Position p) -> do
    e <- exists (cast e @Selected)
    liftIO$ SDL.rendererDrawColor renderer $= if e then V4 255 255 255 255 else V4 255 0 0 255
    SDL.drawPoint renderer (P (round <$> p))

  liftIO$ SDL.rendererDrawColor renderer $= V4 255 255 255 255
  r <- getGlobal
  case r of
    Dragging a b -> SDL.drawRect renderer (Just $ SDL.Rectangle (P (round <$> a)) (round <$> b-a))
    _ -> return ()

  SDL.present renderer

step = do
  let speed :: Num a => a
      speed = 5
      stepPosition :: (Target, Position) -> Safe (Target, Position)
      stepPosition (Target t, Position p)
        | norm (p-t) < speed = Safe (Nothing, Just (Position t))
        | otherwise               = Safe (Just (Target t), Just (Position (p + speed * normalize (t-p))))

  cmap' stepPosition

  m <- getGlobal
  case m of
    Rest -> return ()
    Dragging (V2 ax ay) (V2 bx by) -> do
      resetStore (Proxy @Selected)
      let f :: Position -> Safe Selected
          f (Position (V2 x y)) = Safe (x >= min ax bx && x <= max ax bx && y >= min ay by && y <= max ay by)
      rmap' f

handleEvents = do
  events <- fmap SDL.eventPayload <$> SDL.pollEvents
  mapM_ handleEvent events
  return (SDL.QuitEvent `elem` events)
  where
    handleEvent :: SDL.EventPayload -> System' ()
    handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (P p))) =
      let p' = fromIntegral <$> p in setGlobal (Dragging p' p')

    handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ _)) =
      setGlobal Rest

    handleEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (P p) _)) = do
      md <- getGlobal
      case md of
        Rest         -> return ()
        Dragging a _ -> setGlobal (Dragging a (fromIntegral <$> p))

    handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonRight _ (P (V2 px py)))) = do
      sl :: Slice Selected <- owners
      let r = (*3) . subtract 1 . sqrt . fromIntegral$ S.size sl

      S.forM_ sl $ \e -> do
        dx <- liftIO$ randomRIO (-r,r)
        dy <- liftIO$ randomRIO (-r,r)
        set e (Target (V2 (fromIntegral px+dx) (fromIntegral py+dy)))

    handleEvent _ = return ()

initRenderer = liftIO$ do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  window <- SDL.createWindow "Apecs tutorial" SDL.defaultWindow {SDL.windowInitialSize = V2 hres vres}
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) (SDL.RendererConfig SDL.AcceleratedRenderer False)
  return (window, renderer)

cleanup (window, renderer) = liftIO$ do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

main :: IO ()
main = do
  w <- World <$> initStore <*> initStore <*> initStore <*> initStore <*> initStore
  runSystem game w
