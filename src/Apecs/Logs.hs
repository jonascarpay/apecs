{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}

module Apecs.Logs
  ( Log(..), PureLog(..), FromPure(..), Logger, getLog,
    LVec1, LVec2, LVec3,
  ) where

import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.IntSet as S
import Control.Monad.Reader

import Apecs.Types
import Apecs.Stores
import Apecs.Slice

-- | A PureLog is a piece of state @l c@ that is updated when components @c@ are written or destroyed.
--   Note that @l :: * -> *@
class PureLog l c where
  logEmpty :: l c
  logOnSet :: Entity a -> Maybe c -> c -> l c -> l c
  logOnDestroy :: Entity a -> c -> l c -> l c

-- | An Log is a PureLog with mutable state.
class Log l c where
  ioLogEmpty     :: IO (l c)
  ioLogOnSet     :: l c -> Entity a -> Maybe c -> c -> IO ()
  ioLogOnDestroy :: l c -> Entity a -> c -> IO ()
  ioLogReset     :: l c -> IO ()

class HasLog s l where
  explGetLog :: s -> l (Stores s)

instance HasLog (Logger l s) l where
  {-# INLINE explGetLog #-}
  explGetLog (Logger l _) = l

getLog :: forall w c l. (IsRuntime c, Has w c, HasLog (Storage c) l, Log l c) => System w (l c)
getLog = do s :: Storage c <- getStore
            return (explGetLog s)


-- | FromPure turns a PureLog into a Log
newtype FromPure l c = FromPure (IORef (l c))
instance PureLog l c => Log (FromPure l) c where
  {-# INLINE ioLogEmpty #-}
  ioLogEmpty = FromPure <$> newIORef logEmpty
  {-# INLINE ioLogOnSet #-}
  ioLogOnSet (FromPure lref) e old new = modifyIORef' lref (logOnSet e old new)
  {-# INLINE ioLogOnDestroy #-}
  ioLogOnDestroy (FromPure lref) e c = modifyIORef' lref (logOnDestroy e c)
  {-# INLINE ioLogReset #-}
  ioLogReset (FromPure lref) = writeIORef lref logEmpty

-- | A Logger l of some store updates the Log l with the writes and deletes to Store s
data Logger l s = Logger (l (Stores s)) s

instance (Log l (Stores s), Cachable s) => Initializable (Logger l s) where
  type InitArgs (Logger l s) = InitArgs s
  initStoreWith args = Logger <$> ioLogEmpty <*> initStoreWith args

instance (Log l (Stores s), Cachable s) => HasMembers (Logger l s) where
  {-# INLINE explDestroy #-}
  explDestroy (Logger l s) ety = do
    mc <- explGet s ety
    case mc of
      Just c -> ioLogOnDestroy l (Entity ety) c >> explDestroy s ety
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

instance (Log l (Stores s), Cachable s) => Store (Logger l s) where
  type SafeRW (Logger l s) = SafeRW s
  type Stores (Logger l s) = Stores s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Logger _ s) ety = explGetUnsafe s ety
  {-# INLINE explGet #-}
  explGet (Logger _ s) ety = explGet s ety
  {-# INLINE explSet #-}
  explSet (Logger l s) ety x = do
    mc <- explGet s ety
    ioLogOnSet l (Entity ety) mc x
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

newtype LVec1 l c = LVec1 (l c)
instance Log l c => Log (LVec1 l) c where
  ioLogEmpty = LVec1 <$> ioLogEmpty
  ioLogOnSet     (LVec1 l) e old new = ioLogOnSet l e old new
  ioLogOnDestroy (LVec1 l) e c       = ioLogOnDestroy l e c
  ioLogReset     (LVec1 l)           = ioLogReset l

data LVec2 l1 l2 c = LVec2 (l1 c) (l2 c)
instance (Log l1 c, Log l2 c) => Log (LVec2 l1 l2) c where
  ioLogEmpty = LVec2 <$> ioLogEmpty <*> ioLogEmpty
  ioLogOnSet     (LVec2 l1 l2) e old new = ioLogOnSet l1 e old new >> ioLogOnSet l2 e old new
  ioLogOnDestroy (LVec2 l1 l2) e c       = ioLogOnDestroy l1 e c >> ioLogOnDestroy l2 e c
  ioLogReset     (LVec2 l1 l2)           = ioLogReset l1 >> ioLogReset l2

data LVec3 l1 l2 l3 c = LVec3 (l1 c) (l2 c) (l3 c)
instance (Log l1 c, Log l2 c, Log l3 c) => Log (LVec3 l1 l2 l3) c where
  ioLogEmpty = LVec3 <$> ioLogEmpty <*> ioLogEmpty <*> ioLogEmpty
  ioLogOnSet (LVec3 l1 l2 l3) e old new = do
    ioLogOnSet l1 e old new
    ioLogOnSet l2 e old new
    ioLogOnSet l3 e old new
  ioLogOnDestroy (LVec3 l1 l2 l3) e c = do
    ioLogOnDestroy l1 e c
    ioLogOnDestroy l2 e c
    ioLogOnDestroy l3 e c
  ioLogReset (LVec3 l1 l2 l3) = do
    ioLogReset l1
    ioLogReset l2
    ioLogReset l3

newtype EnumTable c = EnumTable (VM.IOVector S.IntSet)
instance (Bounded c, Enum c) => Log EnumTable c where
  ioLogEmpty = do
    let lo = fromEnum (minBound :: c)
        hi = fromEnum (maxBound :: c)

    if lo == 0
       then EnumTable <$> VM.replicate (hi+1) mempty
       else error "Attempted to initialize EnumTable for a component with a non-zero minBound"

  ioLogOnSet (EnumTable vec) (Entity e) old new = do
    case old of
      Nothing -> return ()
      Just c -> VM.modify vec (S.delete e) (fromEnum c)
    VM.modify vec (S.insert e) (fromEnum new)

  ioLogOnDestroy (EnumTable vec) (Entity e) c = VM.modify vec (S.delete e) (fromEnum c)

  ioLogReset (EnumTable vec) = forM_ [0..VM.length vec - 1] (\e -> VM.write vec e mempty)

-- | Query the @EnumTable@ by an index (the result of @fromEnum@).
--   Will return an empty slice if @index < 0@ of @index >= fromEnum (maxBound)@
{-# INLINE byIndex #-}
byIndex :: EnumTable c -> Int -> System w (Slice c)
byIndex (EnumTable vec) c
  | c < 0                  = return mempty
  | c >= VM.length vec - 1 = return mempty
  | otherwise = liftIO$ sliceFromList . S.toList <$> VM.read vec c
