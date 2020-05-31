{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Experimental.Pool
  (
  )
where

import           Apecs.Core
import           Apecs.Experimental.Stores   (VoidStore)
import           Apecs.System
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits                   (shiftL, (.&.))
import           Data.Functor
import qualified Data.IntSet                 as IS
import           Data.IORef
import           Data.Proxy                  (Proxy (..))
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.Generics
import           GHC.TypeLits                (KnownNat, Nat, natVal)

class Unit a where
  unit :: a
  default unit :: (Generic a, Rep a ~ D1 m (C1 n U1)) => a
  unit = to (M1 (M1 U1))

-- | A set of entities, based on 'IS.IntSet'.
--   Intended to be used with flags/tags/unit types.
--   Does not contain values, so `explGet` will always yield `mempty`, regardless of what it was set to.
newtype EtySet s = EtySet (IORef IS.IntSet)

type instance Elem (EtySet s) = s

instance MonadIO m => ExplInit m (EtySet s) where
  explInit = liftIO $ EtySet <$> newIORef mempty

instance (Unit s, MonadIO m) => ExplGet m (EtySet s) where
  explExists (EtySet s) ety = liftIO $ IS.member ety <$> readIORef s
  explGet _ _ = pure unit

instance MonadIO m => ExplSet m (EtySet s) where
  explSet (EtySet s) ety _ = liftIO $ modifyIORef' s $ IS.insert ety

instance MonadIO m => ExplMembers m (EtySet s) where
  explMembers (EtySet s) = liftIO $ U.fromList . IS.toList <$> readIORef s

instance MonadIO m => ExplDestroy m (EtySet s) where
  explDestroy (EtySet s) ety = liftIO $ modifyIORef' s $ IS.delete ety

data Pool (n :: Nat) a
  = Pool
      { _bitmask   :: Int,
        _set       :: UM.IOVector Bool,
        _queue     :: UM.IOVector Int,
        _queueHead :: IORef Int,
        _queueTail :: IORef Int
      }

{-# INLINE poolPop #-}
poolPop :: Pool n a -> IO (Maybe Int)
poolPop (Pool mask flags queue qh qt) = do
  h <- readIORef qh
  t <- readIORef qt
  if h == t
    then pure Nothing
    else do
      n <- UM.read queue h
      UM.write flags n True
      writeIORef qh (succ h .&. mask)
      pure (Just n)

{-# INLINE poolPush #-}
poolPush :: Pool n a -> Int -> IO ()
poolPush (Pool mask actives q _ qt) n = do
  active <- UM.read actives n
  when active $ do
    UM.write actives n False
    t <- readIORef qt
    UM.write q t n
    writeIORef qt (succ t .&. mask)

nextPoolEntity :: forall w m n a. (Has w m a, Storage a ~ Pool n a, MonadIO m) => a -> SystemT w m (Maybe Entity)
nextPoolEntity _ = do
  (s :: Pool n a) <- getStore
  liftIO $ fmap Entity <$> poolPop s

newPoolEntity :: forall w m n c a. (Has w m a, Storage a ~ Pool n a, Set w m c, MonadIO m) => a -> c -> SystemT w m (Maybe Entity)
newPoolEntity p c = do
  mn <- nextPoolEntity p
  case mn of
    Just n  -> set n c $> Just n
    Nothing -> pure Nothing

type instance Elem (Pool n a) = a

instance (MonadIO m, KnownNat n) => ExplInit m (Pool n a) where
  {-# INLINE explInit #-}
  explInit = liftIO $ do
    let n = fromIntegral $ natVal (Proxy @n) :: Int
        size = head . dropWhile (< n) $ iterate (`shiftL` 1) 1
        mask = size - 1
    flags <- UM.replicate size False
    queue <- UM.unsafeNew size
    forM_ [0 .. size - 1] $ \i -> UM.write queue i i
    qhead <- newIORef 0
    qtail <- newIORef (size - 1)
    pure $ Pool mask flags queue qhead qtail

instance (Unit a, MonadIO m) => ExplGet m (Pool n a) where
  {-# INLINE explGet #-}
  explGet _ _ = pure unit
  {-# INLINE explExists #-}
  explExists (Pool mask flags _ _ _) ety =
    if mask .&. ety == ety
       then liftIO $ UM.unsafeRead flags ety
       else pure False

instance MonadIO m => ExplDestroy m (Pool n a) where
  explDestroy p ety = liftIO $ poolPush p ety

instance MonadIO m => ExplMembers m (Pool n a) where
  explMembers (Pool mask flags _ rqh rqt) = liftIO $ do
    qh <- readIORef rqh
    qt <- readIORef rqt
    let n = (if qh < qt then qh else qh + mask + 1) - qt
    U.fromListN n <$> filterM (UM.read flags) [0 .. mask]
