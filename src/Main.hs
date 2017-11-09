{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Apecs
import           Apecs.TH
import           Apecs.Types
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

makeWorld "World" [''Body]
type System' a = System World a

-- TODO: enforce:
--    Cannot set mass of non-dynamic body
--    Cannot simulate when mass <= 0
--    Cannot simulate when moment <= 0

game = do
  e <- newEntity (DynamicBody, Moment 1, Mass 1)
  cmapM_ $ \(Position p) -> liftIO (print p)
  stepPhysicsSys (1/60)
  cmapM_ $ \(Position p) -> liftIO (print p)
  writeGlobal (Gravity (V2 0 (-10)))
  stepPhysicsSys (1/60)
  cmapM_ $ \(Position p) -> liftIO (print p)

main :: IO ()
main = do initWorld >>= runSystem game
          performMajorGC

