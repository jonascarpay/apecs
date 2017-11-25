{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics                    as P
import           Apecs.Physics.Gloss
import           Graphics.Gloss                   as G
import           Graphics.Gloss.Interface.IO.Game as G

data Target = Target
instance Component Target
  where type Storage Target = Unique Target

makeWorld "World" [''Physics, ''BodyPicture, ''GlossView, ''Target]

initialize :: System World ()
initialize = do
  setGlobal $ GlossView 0 20
  setGlobal $ Gravity (V2 0 (-10))

  let line = Segment (V2 (-30) 0, V2 30 0) 0
      ball = P.Circle 0 1

  newEntity ( StaticBody
            , Shape line
            , BodyPicture (color white $ fromShape line)
            , Position (V2 4 0))

  newEntity ( DynamicBody
            , Shape ball
            , BodyPicture (color red $ fromShape ball)
            , Density 1)

  newEntity ( StaticBody, Target )

  return ()

handle :: Event -> System World ()

handle (EventKey (MouseButton LeftButton) Down _ (x,y)) = do
  view <- getGlobal
  let mpos = mouseToWorld view (V2 (realToFrac x) (realToFrac y))
  return ()
  pq <- pointQuery mpos 1 defaultFilter
  case pq of
    Nothing -> return ()
    _       -> undefined

handle _ = return ()

main = do
  w <- initWorld
  runSystem initialize w
  playIO FullScreen black 60 w render handler stepper
    where
      render w        = runSystem drawWorld w
      handler event w = runSystem (handle event) w >> return w
      stepper dT w    = runSystem (stepPhysics (realToFrac dT)) w >> return w

