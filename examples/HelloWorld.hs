{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Apecs.Physics
import           Apecs.Physics.Gloss
import           Control.Monad

makeWorld "World" [''Physics, ''Camera]

initialize = do
  set global ( Camera (V2 0 1) 60
             , earthGravity )

  lineBody <- newEntity (StaticBody, Angle (-pi/20))
  newEntity (Shape lineBody (hLine 6), Elasticity 0.9)

  ball <- newEntity (DynamicBody, Position (V2 0 3))
  newEntity (Shape ball (cCircle 0.5), Density 1, Elasticity 0.9)

disp = InWindow "Hello World" (640,640) (10,10)
main = initWorld >>= runSystem (initialize >> simulate disp)
