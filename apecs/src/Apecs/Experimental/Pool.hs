{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module Apecs.Experimental.Pool
  ( Active,
    FiniteStore,
  )
where

import Apecs.Core
import Apecs.Experimental.Stores (VoidStore)
import Apecs.Stores (Cache, Global, Unique)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits ((.&.), shiftL)
import Data.IORef
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.TypeLits (KnownNat, Nat, natVal, natVal)

data Active = Active

data Pool (n :: Nat)
  = Pool
      { bitmask :: Int,
        alive :: UM.IOVector Bool,
        queue :: UM.IOVector Int,
        queueHead :: IORef Int,
        queueTail :: IORef Int
      }

instance (MonadIO m, KnownNat n) => ExplInit m (Pool n) where
  explInit = do
    let n = fromIntegral $ natVal (Proxy @n) :: Int
        size = head . dropWhile (< n) $ iterate (`shiftL` 1) 1
        mask = size - 1
    alive <- undefined
    undefined

popInactive :: Pool n -> IO (Maybe Int)
popInactive (Pool mask active queue qh qt) = do
  h <- readIORef qh
  t <- readIORef qt
  if h == t
    then pure Nothing
    else do
      n <- UM.read queue h
      UM.write active n True
      writeIORef qh (succ h .&. mask)
      pure (Just n)

pushInactive :: Int -> Pool n -> IO ()
pushInactive n (Pool mask actives q qh qt) = do
  active <- UM.read actives n
  when active $ do
    UM.write actives n False
    t <- readIORef qt
    UM.write q t n
    writeIORef qt (succ t .&. mask)

class FiniteStore s

instance FiniteStore (Cache n s)

instance FiniteStore (Unique s)

instance FiniteStore (Global s)

instance FiniteStore (VoidStore s)
