{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Apecs.Physics
import           Apecs.Physics.Gloss
import           Control.Monad
import           Graphics.Gloss

makeWorld "World" [''Physics, ''BodyPicture, ''Camera]

initialize = do
  setGlobal ( Camera (V2 0 1) 60
            , earthGravity )

  let lineShape = hLine 6
  newEntity ( StaticBody
            , Angle (-pi/20)
            , Shape lineShape
            , Elasticity 0.9
            , BodyPicture . color white . toPicture $ lineShape )

  let ballShape = cCircle 0.5
  newEntity ( DynamicBody
            , Shape ballShape
            , Position (V2 0 3)
            , Density 1
            , Elasticity 0.9
            , BodyPicture . color red . toPicture $ ballShape )

main = do
  w <- initWorld
  runSystem initialize w
  defaultSimulate w "Hello World"

