{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Apecs
import           Apecs.TH
import           Apecs.Types
import           Control.Monad
import qualified Data.IntMap               as M
import           Data.IORef
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Foreign.ForeignPtr
import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types          as C
import qualified Language.Haskell.TH       as TH
import           Linear.V2
import           System.Mem

import           Instances
import           Shape
import           Types

context phycsCtx
include "<chipmunk.h>"

makeWorld "World" [''Body]
type System' a = System World a

-- TODO: enforce:
--    Cannot set mass of non-dynamic body
--    Cannot simulate when mass <= 0
--    Cannot simulate when moment <= 0

{--}
game = do
  let ball = Shape (Circle 0 1) defaultProperties

  e <- newEntity (DynamicBody, Shapes [ball], Position (V2 0 (-10)))

  (Safe (Just m) :: Safe Mass) <- get (cast e) -- TODO cast??

  liftIO$ print m
  cmapM_ $ \(Position p) -> liftIO (print p)
  stepPhysicsSys (1/60)
  cmapM_ $ \(Position p) -> liftIO (print p)
  writeGlobal (Gravity (V2 0 (-10)))
  replicateM_ 100 $ stepPhysicsSys (1/60)
  cmapM_ $ \(Position p) -> liftIO (print p)
  stepPhysicsSys (1/60)
  cmapM_ $ \(Position p) -> liftIO (print p)

main = initWorld >>= runSystem game
--}

{--
main :: IO ()
main = [block| void {
  cpSpace* space = cpSpaceNew();
  cpSpaceSetGravity(space, cpv(0,-100));

  cpShape *ground = cpSegmentShapeNew(cpSpaceGetStaticBody(space), cpv(-20,5), cpv(20, -5), 0);
  cpShapeSetFriction(ground, 1);
  cpSpaceAddShape(space, ground);

  cpBody *ballBody = cpSpaceAddBody(space, cpBodyNew(1,1));
  cpBodySetPosition(ballBody, cpv(0,15));
  printf("Mass: %f\n", cpBodyGetMass(ballBody));
  cpShape *ballShape = cpSpaceAddShape(space, cpCircleShapeNew(ballBody, 1, cpvzero));
  cpShapeSetFriction(ballShape, 0.7);
  cpShapeSetDensity(ballShape,1);
  printf("Mass: %f\n", cpBodyGetMass(ballBody));

  printf("%f\n", cpBodyGetPosition(ballBody).y);
  cpFloat dT = 1.0/60.0;
  cpSpaceStep(space, dT);
  cpSpaceStep(space, dT);
  cpSpaceStep(space, dT);
  cpSpaceStep(space, dT);
  cpSpaceStep(space, dT);
  cpSpaceStep(space, dT);
  cpSpaceStep(space, dT);
  printf("%f\n", cpBodyGetPosition(ballBody).y);
  printf("%f\n", cpSpaceGetGravity(space).y);
  }|]

--}
