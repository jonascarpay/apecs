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

  newEntity ( StaticBody
            , Angle (-pi/20)
            , Shape (hLine 6)
            , Elasticity 0.9 )

  newEntity ( DynamicBody
            , Shape (cCircle 0.5)
            , Position (V2 0 3)
            , Density 1
            , Elasticity 0.9 )

disp = InWindow "Hello World" (640,640) (10,10)
main = initWorld >>= runSystem (initialize >> simulate disp)
