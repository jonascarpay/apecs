{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics                        as P
import           Apecs.Physics.Gloss
import           Apecs.Util
import           Control.Monad
import           Graphics.Gloss                       as G
import           System.Random

makeWorld "World" [''Physics, ''BodyPicture, ''Camera]

initialize :: System World ()
initialize = do
  setGlobal ( Camera 0 50
            , earthGravity )

  let sides = toEdges $ cRectangle 5
  tumbler <- newEntity ( KinematicBody
                       , AngularVelocity (-1)
                       , BodyPicture . color white . foldMap toPicture $ sides )

  forM_ sides $ newEntity . ShapeExtend (cast tumbler) . setRadius 0.05

  replicateM_ 200 $ do
    x <- liftIO$ randomRIO (-2, 2)
    y <- liftIO$ randomRIO (-2, 2)
    r <- liftIO$ randomRIO (0.1, 0.2)
    let ballshape = cCircle r
    let c = (realToFrac x+2)/3
    newEntity ( DynamicBody
              , Position (V2 x y)
              , Shape ballshape
              , BodyPicture . color (makeColor 1 c c 1) . toPicture $ ballshape
              , Density 1 )

  return ()

main = do
  w <- initWorld
  runSystem initialize w
  defaultSimulate w "Tumbler"

