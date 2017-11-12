{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Monad

import           Apecs.Physics
import           Apecs.Physics.Render

makeWorld "World" [''Color, ''Physics]

chain :: (Entity a) -> Int -> System World (Entity ())
chain init links = go (cast init) 0
  where
    go a n = do
      b <- newEntity ( DynamicBody
                     , Position (V2 (fromIntegral n) 5)
                     , Shape (Segment 0 (V2 1 0) 0.1) defaultProperties )

      newEntity $ Constraint (cast a) (cast b) (PivotJoint (V2 (fromIntegral n) 5))
      if (n >= links) then return (cast b) else go b (n+1)

initialize = do
  setGlobal $ Gravity (V2 0 (-10))
  fixed <- newEntity StaticBody
  b <- chain fixed 8

  ball <- newEntity ( DynamicBody
                    , Position (V2 10 5)
                    , Shape (Circle 0 1) defaultProperties
                    , red )

  newEntity $ Constraint (cast b) (cast ball) (PivotJoint (V2 9 5) )
  return ()

main = simulateWorld (InWindow "chain" (640,480) (10,10)) 30 initWorld initialize
