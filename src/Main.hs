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

import Context
import Instances

makeWorld "World" [''Body]
type System' a = System World a

game = do newEntity (Position 0)
          return ()

main :: IO ()
main = initWorld >>= runSystem game
