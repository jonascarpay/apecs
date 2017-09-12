{-# LANGUAGE Strict, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Apecs.Util
  ( runGC, initStore,
    EntityCounter, initCounter, nextEntity, newEntity,
    ConcatQueries(..),
  ) where

import System.Mem (performMajorGC)
import Control.Monad.Reader (liftIO)

import Apecs.Core
import Apecs.Stores

newtype EntityCounter = EntityCounter Int
instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

initCounter :: IO (Storage EntityCounter)
initCounter = initStoreWith (EntityCounter 0)

{-# INLINE nextEntity #-}
nextEntity :: Has w EntityCounter => System w (Entity ())
nextEntity = do EntityCounter n <- readGlobal
                writeGlobal (EntityCounter (n+1))
                return (Entity n)

{-# INLINE newEntity #-}
newEntity :: (IsRuntime c, Has w c, Has w EntityCounter)
          => c -> System w (Entity c)
newEntity c = do ety <- nextEntity
                 set (cast ety) c
                 return (cast ety)

runGC :: System w ()
runGC = liftIO performMajorGC

initStore :: (Initializable s, InitArgs s ~ ()) => IO s
initStore = initStoreWith ()

newtype ConcatQueries q = ConcatQueries [q]
instance Query q s => Query (ConcatQueries q) s where
  explSlice s (ConcatQueries qs) = mconcat <$> traverse (explSlice s) qs

