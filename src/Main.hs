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
import qualified Data.IntMap                          as M
import           Data.IORef
import qualified Data.Map                             as Map
import           Data.Monoid                          ((<>))
import           Foreign.ForeignPtr
import           Graphics.Gloss.Interface.IO.Simulate as G
import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types                     as C
import qualified Language.Haskell.TH                  as TH
import           Linear.V2
import           System.Mem

import           Body
import           Instances
import           Render
import           Shape
import           Types                                as P

context phycsCtx
include "<chipmunk.h>"

makeWorld "World" [''Physics]
type System' a = System World a

-- TODO: enforce:
--    Cannot set mass of non-dynamic body
--    Cannot simulate when mass <= 0
--    Cannot simulate when moment <= 0

game = do
  writeGlobal (Gravity (V2 0 (-10)))
  let ball = Shape (P.Circle 0 1) defaultProperties
  newEntity (DynamicBody, Shapes [ball])
  return ()


main = simulateWorld FullScreen initWorld game
