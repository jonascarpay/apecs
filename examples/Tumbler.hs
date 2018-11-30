{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics
import           Apecs.Physics.Gloss
import           Apecs.Util
import           Control.Monad
import           System.Random

makeWorld "World" [''Physics, ''Camera]

initialize :: System World ()
initialize = do
  set global ( Camera 0 50
             , earthGravity )

  let sides = toEdges $ cRectangle 5
  tumbler <- newEntity ( KinematicBody
                       , AngularVelocity (-1)
                       )

  forM_ sides $ newEntity . Shape tumbler . setRadius 0.05

  replicateM_ 200 $ do
    x <- liftIO$ randomRIO (-2, 2)
    y <- liftIO$ randomRIO (-2, 2)
    r <- liftIO$ randomRIO (0.1, 0.2)
    let c = (realToFrac x+2)/3
    ball <- newEntity (DynamicBody, Position (V2 x y))
    newEntity (Shape ball (cCircle r), Density 1 )

disp = InWindow "Tumbler" (640,640) (10,10)
main = initWorld >>= runSystem (initialize >> simulate disp)
