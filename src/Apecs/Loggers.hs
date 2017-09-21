{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Apecs.Loggers where

import Apecs.Types hiding (Query)
import Apecs.Stores
import Data.IORef

class Log l where
  type LogComponent l
  logEmpty :: l
  logOnSet :: Entity a -> Maybe (LogComponent l) -> LogComponent l -> l -> l
  logOnDestroy :: Entity a -> LogComponent l -> l -> l

class IOLog l where
  type IOLogComponent l
  ioLogEmpty :: IO l
  ioLogOnSet :: l -> Int -> Maybe (IOLogComponent l) -> IOLogComponent l -> IO ()
  ioLogOnDestroy :: l -> Int -> IOLogComponent l -> IO ()
  ioLogReset :: l -> IO ()

newtype FromPure l = FromPure (IORef l)
instance Log l => IOLog (FromPure l) where
  type IOLogComponent (FromPure l) = LogComponent l
  {-# INLINE ioLogEmpty #-}
  ioLogEmpty = FromPure <$> newIORef logEmpty
  {-# INLINE ioLogOnSet #-}
  ioLogOnSet (FromPure lref) e old new = modifyIORef' lref (logOnSet (Entity e) old new)
  {-# INLINE ioLogOnDestroy #-}
  ioLogOnDestroy (FromPure lref) e c = modifyIORef' lref (logOnDestroy (Entity e) c)
  {-# INLINE ioLogReset #-}
  ioLogReset (FromPure lref) = writeIORef lref logEmpty

data Logger l s = Logger l s

class HasLog s l where
  explGetLog :: s -> l

instance HasLog (Logger l s) l where explGetLog (Logger l _) = l

instance HasLog s l => HasLog (Logger la s) l where explGetLog (Logger _ s) = explGetLog s

getLog :: forall w l. (HasLog (Storage (IOLogComponent l)) l, Has w (IOLogComponent l)) => System w l
getLog = explGetLog <$> (getStore :: System w (Storage (IOLogComponent l)))


instance (IOLog l, Cachable s) => Initializable (Logger l s) where
  type InitArgs (Logger l s) = InitArgs s
  initStoreWith args = Logger <$> ioLogEmpty <*> initStoreWith args

instance (IOLogComponent l ~ Stores s, IOLog l, Cachable s) => HasMembers (Logger l s) where
  {-# INLINE explDestroy #-}
  explDestroy (Logger l s) ety = do
    mc <- explGet s ety
    case mc of
      Just c -> ioLogOnDestroy l ety c >> explDestroy s ety
      _ -> return ()

  {-# INLINE explExists #-}
  explExists (Logger _ s) ety = explExists s ety
  {-# INLINE explMembers #-}
  explMembers (Logger _ s) = explMembers s
  {-# INLINE explReset #-}
  explReset (Logger l s) = ioLogReset l >> explReset s
  {-# INLINE explImapM_ #-}
  explImapM_ (Logger _ s) = explImapM_ s
  {-# INLINE explImapM #-}
  explImapM (Logger _ s) = explImapM s

instance (IOLogComponent l ~ Stores s, IOLog l, Cachable s) => Store (Logger l s) where
  type SafeRW (Logger l s) = SafeRW s
  type Stores (Logger l s) = Stores s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Logger _ s) ety = explGetUnsafe s ety
  {-# INLINE explGet #-}
  explGet (Logger _ s) ety = explGet s ety
  {-# INLINE explSet #-}
  explSet (Logger l s) ety x = do
    mc <- explGet s ety
    ioLogOnSet l ety mc x
    explSet s ety x

  {-# INLINE explSetMaybe #-}
  explSetMaybe s ety (Nothing) = explDestroy s ety
  explSetMaybe s ety (Just x) = explSet s ety x

  {-# INLINE explModify #-}
  explModify (Logger l s) ety f = do
    mc <- explGet s ety
    case mc of
      Just c -> explSet (Logger l s) ety (f c)
      Nothing -> return ()

  {-# INLINE explCmapM_ #-}
  explCmapM_  (Logger _ s) = explCmapM_  s
  {-# INLINE explCmapM #-}
  explCmapM   (Logger _ s) = explCmapM   s
  {-# INLINE explCimapM_ #-}
  explCimapM_ (Logger _ s) = explCimapM_ s
  {-# INLINE explCimapM #-}
  explCimapM  (Logger _ s) = explCimapM  s
