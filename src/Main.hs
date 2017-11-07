{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Linear.V2
import Apecs
import Apecs.Types
import Apecs.TH
import Data.Monoid ((<>))
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.IORef
import Foreign.ForeignPtr
import Language.C.Inline
import Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import System.Mem

import Context
import Instances

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

