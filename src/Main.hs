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
import qualified Graphics.Gloss            as G
import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types          as C
import qualified Language.Haskell.TH       as TH
import           Linear.V2
import           System.Mem

import           Body
import           Instances
import           Render
import           Shape
import           Types

-- TODO: enforce:
--    Cannot set mass of non-dynamic body
--    Cannot simulate when mass <= 0
--    Cannot simulate when moment <= 0

makeWorld "World" [''Physics]

initialize = do
  writeGlobal (Gravity (V2 0 (-10)))

  let ball = Shape (Circle 0 0.2) defaultProperties {elasticity = 0.8}
      line = Shape (Segment (V2 (-1) 0) (V2 1 0) 0) defaultProperties {elasticity = 0.8}

  newEntity (DynamicBody, shape ball, Position (V2 0 2))
  newEntity (StaticBody,  shape line, Position (V2 0 (-1)), Angle (-pi/10))

main = simulateWorld (G.InWindow "phycs" (640,480) (10,10)) 100 initWorld initialize
