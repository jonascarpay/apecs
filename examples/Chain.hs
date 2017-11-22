{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Monad

import           Apecs.Physics
import           Apecs.Physics.Render

makeWorld "World" [''Color, ''Physics]

initialize = do
    setGlobal $ Gravity (V2 0 (-10))
    fixed <- newEntity StaticBody
    chain (cast fixed) 0
  where
    links = 10
    chain a n = do
      b <- newEntity ( DynamicBody
                     , Position (V2 n 5)
                     , Shape (Segment 0 (V2 1 0) 0.1) defaultProperties )

      newEntity ( Constraint (cast a) (cast b) (PivotJoint (V2 n 5))
                , CollideBodies False
                , MaxBias 1 )

      unless (n >= links) $ chain b (n+1)

main = simulateWorld (InWindow "chain" (640,480) (10,10)) 30 initWorld initialize
