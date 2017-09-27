{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}

module Apecs.Logs
  ( -- * Types and classes
    Log(..), PureLog(..), FromPure(..), Logger, getLog, readIORef,
    LVec1, LVec2, LVec3,

    -- * EnumTable
    EnumTable, byIndex, byEnum,
  ) where

import Data.IORef
import qualified Data.Vector.Mutable as VM
import qualified Data.IntSet as S
import Control.Monad.Reader

import Apecs.Types
import Apecs.Stores
import qualified Apecs.Slice as Sl

-- | A PureLog is a piece of state @l c@ that is updated when components @c@ are written or destroyed.
--   Note that @l :: * -> *@
class PureLog l c where
  pureEmpty :: l c
  pureOnSet :: Entity a -> Maybe c -> c -> l c -> l c
  pureOnDestroy :: Entity a -> c -> l c -> l c

-- | A Log is a PureLog with mutable state.
class Log l c where
  logEmpty     :: IO (l c)
  logOnSet     :: l c -> Entity a -> Maybe c -> c -> IO ()
  logOnDestroy :: l c -> Entity a -> c -> IO ()
  logReset     :: l c -> IO ()

class HasLog s l where
  explGetLog :: s -> l (Stores s)

instance HasLog (Logger l s) l where
  {-# INLINE explGetLog #-}
  explGetLog (Logger l _) = l

-- | Produces the log indicated by the return type.
{-# INLINE getLog #-}
getLog :: forall w c l. (Store (Storage c), Has w c, HasLog (Storage c) l, Log l c) => System w (l c)
getLog = do s :: Storage c <- getStore
            return (explGetLog s)


-- | FromPure turns a PureLog into a Log
newtype FromPure l c = FromPure (IORef (l c))
instance PureLog l c => Log (FromPure l) c where
  {-# INLINE logEmpty #-}
  logEmpty = FromPure <$> newIORef pureEmpty
  {-# INLINE logOnSet #-}
  logOnSet (FromPure lref) e old new = modifyIORef' lref (pureOnSet e old new)
  {-# INLINE logOnDestroy #-}
  logOnDestroy (FromPure lref) e c = modifyIORef' lref (pureOnDestroy e c)
  {-# INLINE logReset #-}
  logReset (FromPure lref) = writeIORef lref pureEmpty

-- | A @Logger l@ of some store updates its @Log l@ with the writes and deletes to store @s@
data Logger l s = Logger (l (Stores s)) s

instance (Log l (Stores s), Cachable s) => Store (Logger l s) where
  type Stores (Logger l s) = Stores s
  initStore = Logger <$> logEmpty <*> initStore

  {-# INLINE explDestroy #-}
  explDestroy (Logger l s) ety = do
    mc <- explGet s ety
    case mc of
      Just c -> logOnDestroy l (Entity ety) c >> explDestroy s ety
      _ -> return ()

  {-# INLINE explExists #-}
  explExists (Logger _ s) ety = explExists s ety
  {-# INLINE explMembers #-}
  explMembers (Logger _ s) = explMembers s
  {-# INLINE explReset #-}
  explReset (Logger l s) = logReset l >> explReset s
  {-# INLINE explImapM_ #-}
  explImapM_ (Logger _ s) = explImapM_ s
  {-# INLINE explImapM #-}
  explImapM (Logger _ s) = explImapM s

  type SafeRW (Logger l s) = SafeRW s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Logger _ s) ety = explGetUnsafe s ety
  {-# INLINE explGet #-}
  explGet (Logger _ s) ety = explGet s ety
  {-# INLINE explSet #-}
  explSet (Logger l s) ety x = do
    mc <- explGet s ety
    logOnSet l (Entity ety) mc x
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

-- | Composite Log consisting of 1 Log
newtype LVec1 l c = LVec1 (l c)
instance Log l c => Log (LVec1 l) c where
  {-# INLINE logEmpty #-}
  logEmpty = LVec1 <$> logEmpty
  {-# INLINE logOnSet #-}
  logOnSet     (LVec1 l) e old new = logOnSet l e old new
  {-# INLINE logOnDestroy #-}
  logOnDestroy (LVec1 l) e c       = logOnDestroy l e c
  {-# INLINE logReset #-}
  logReset     (LVec1 l)           = logReset l

-- | Composite Log consisting of 2 Logs
data LVec2 l1 l2 c = LVec2 (l1 c) (l2 c)
instance (Log l1 c, Log l2 c) => Log (LVec2 l1 l2) c where
  {-# INLINE logEmpty #-}
  logEmpty = LVec2 <$> logEmpty <*> logEmpty
  {-# INLINE logOnSet #-}
  logOnSet     (LVec2 l1 l2) e old new = logOnSet l1 e old new >> logOnSet l2 e old new
  {-# INLINE logOnDestroy #-}
  logOnDestroy (LVec2 l1 l2) e c       = logOnDestroy l1 e c >> logOnDestroy l2 e c
  {-# INLINE logReset #-}
  logReset     (LVec2 l1 l2)           = logReset l1 >> logReset l2

-- | Composite Log consisting of 3 Logs
data LVec3 l1 l2 l3 c = LVec3 (l1 c) (l2 c) (l3 c)
instance (Log l1 c, Log l2 c, Log l3 c) => Log (LVec3 l1 l2 l3) c where
  {-# INLINE logEmpty #-}
  logEmpty = LVec3 <$> logEmpty <*> logEmpty <*> logEmpty
  {-# INLINE logOnSet #-}
  logOnSet (LVec3 l1 l2 l3) e old new = do
    logOnSet l1 e old new
    logOnSet l2 e old new
    logOnSet l3 e old new
  {-# INLINE logOnDestroy #-}
  logOnDestroy (LVec3 l1 l2 l3) e c = do
    logOnDestroy l1 e c
    logOnDestroy l2 e c
    logOnDestroy l3 e c
  {-# INLINE logReset #-}
  logReset (LVec3 l1 l2 l3) = do
    logReset l1
    logReset l2
    logReset l3

-- | Hashtable that maintains buckets of entities whose @fromEnum c@ produces the same value
newtype EnumTable c = EnumTable (VM.IOVector S.IntSet)
instance (Bounded c, Enum c) => Log EnumTable c where
  {-# INLINE logEmpty #-}
  logEmpty = do
    let lo = fromEnum (minBound :: c)
        hi = fromEnum (maxBound :: c)

    if lo == 0
       then EnumTable <$> VM.replicate (hi+1) mempty
       else error "Attempted to initialize EnumTable for a component with a non-zero minBound"

  {-# INLINE logOnSet #-}
  logOnSet (EnumTable vec) (Entity e) old new = do
    case old of
      Nothing -> return ()
      Just c -> VM.modify vec (S.delete e) (fromEnum c)
    VM.modify vec (S.insert e) (fromEnum new)

  {-# INLINE logOnDestroy #-}
  logOnDestroy (EnumTable vec) (Entity e) c = VM.modify vec (S.delete e) (fromEnum c)

  {-# INLINE logReset #-}
  logReset (EnumTable vec) = forM_ [0..VM.length vec - 1] (\e -> VM.write vec e mempty)

-- | Query the @EnumTable@ by an index (the result of @fromEnum@).
--   Will return an empty slice if @index < 0@ of @index >= fromEnum (maxBound)@.
{-# INLINE byIndex #-}
byIndex :: EnumTable c -> Int -> System w (Slice c)
byIndex (EnumTable vec) c
  | c < 0                  = return mempty
  | c >= VM.length vec - 1 = return mempty
  | otherwise = liftIO$ Sl.fromList . S.toList <$> VM.read vec c

-- | Query the @EnumTable@ by an example enum.
--   Will not perform bound checks, so crashes if `fromEnum c < 0 && fromEnum c > fromEnum maxBound `.
byEnum :: Enum c => EnumTable c -> c -> System w (Slice c)
byEnum (EnumTable vec) c = liftIO$ Sl.fromList . S.toList <$> VM.read vec (fromEnum c)
