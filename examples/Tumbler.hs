{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics                        as P
import           Apecs.Physics.Gloss
import           Apecs.Util
import           Control.Monad
import           Data.Mesh
import           Graphics.Gloss                       as G
import           System.Random

makeWorld "World" [''Physics, ''BodyPicture, ''GlossView]

initialize :: System World ()
initialize = do
  setGlobal ( GlossView 0 50
            , earthGravity )

  let mesh = centeredRectangle 5 5

  tumbler <- newEntity ( KinematicBody
                       , AngularVelocity (-1)
                       , BodyPicture . color white . G.Line . fmap v2ToTuple . vertexLoop $ mesh )

  forM_ (edges mesh) $
    newEntity . ShapeExtend (cast tumbler) . flip Segment 0.05

  replicateM_ 200 $ do
    x <- liftIO$ randomRIO (-2, 2)
    y <- liftIO$ randomRIO (-2, 2)
    r <- liftIO$ randomRIO (0.1, 0.2)
    let c = (realToFrac x+2)/3
    newEntity ( DynamicBody
              , Position (V2 x y)
              , Shape (P.Circle 0 r)
              , BodyPicture . color (makeColor 1 c c 1) $ G.Circle (realToFrac r)
              , Density 1 )

  return ()

main = do
  w <- initWorld
  runSystem initialize w
  defaultSimulate w

