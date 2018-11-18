{-# LANGUAGE TypeFamilies #-}

module Apecs.STM where

import           Control.Concurrent.STM      as S
import           Control.Concurrent.STM.TVar as S
import qualified STMContainers.Map           as M

import           Apecs.Core

newtype Map c = Map (M.Map Entity c)

type instance Elem (Map c) = c
instance ExplInit (Map c) where
  explInit = Map <$> M.newIO

instance ExplGet (Map c) where
