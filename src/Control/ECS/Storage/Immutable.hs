module Control.ECS.Storage.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.IORef
import Data.Vector.Unboxed as U

import Control.ECS.Core

newtype Map c = Map {unMap :: IORef (M.IntMap c)}

instance SStorage (Map c) where
  type SSafeElem (Map c) = Maybe c
  type SElem     (Map c) = c

  sEmpty = Map <$> newIORef mempty
  sAll      (Map m) = U.fromList . M.keys <$> readIORef m
  sMember   (Map m) ety = M.member ety <$> readIORef m
  sDestroy  (Map m) ety = modifyIORef' m (M.delete ety)
  sRead (Map m) ety = M.lookup ety <$> readIORef m
  sWrite    m Nothing ety = sDestroy m ety
  sWrite    (Map m) (Just x) ety = modifyIORef' m (M.insert ety x)

  sWriteUnsafe (Map m) x ety = modifyIORef' m (M.insert ety x)
  sReadUnsafe (Map m) ety = do mx <- M.lookup ety <$> readIORef m
                               case mx of
                                 Nothing -> error "Unsafe read miss"
                                 Just x -> return x
  --TODO: Inline decls? (Can be copied from Cache)

newtype FlagSet = FlagSet {unFlagSet :: IORef S.IntSet}

instance SStorage FlagSet where
  type SSafeElem FlagSet = Bool
  type SElem     FlagSet = ()

  sEmpty = FlagSet <$> newIORef mempty
  sAll (FlagSet s) = U.fromList . S.toList <$> readIORef s
  sMember (FlagSet s) ety = S.member ety <$> readIORef s
  sDestroy (FlagSet s) ety = modifyIORef' s (S.delete ety)
  sRead = sMember
  sWrite s False ety = sDestroy s ety
  sWrite (FlagSet s) True ety = modifyIORef' s (S.insert ety)

  sWriteUnsafe s _ e = sWrite s True e
  sReadUnsafe  _ _   = return ()

  --TODO: Inline decls? (Can be copied from Cache)
