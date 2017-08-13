module Control.ECS.Mutable where

import Control.Monad.State
import qualified Data.HashTable.IO as H
import qualified Data.IntSet as S
import Data.IORef

import Control.ECS.Core

data MutableMap c = MutableMap { mmKeys :: IORef S.IntSet, mmTable :: H.BasicHashTable Int c }

instance SStorage (MutableMap c) where
  type SRuntime (MutableMap c) = Maybe c

  sEmpty = do tab  <- liftIO H.new
              keys <- liftIO (newIORef mempty)
              return $ MutableMap keys tab

  sSlice = do ref <- mmKeys <$> get
              fmap Entity . S.toList <$> liftIO (readIORef ref)


  sRetrieve (Entity e) = do tab <- mmTable <$> get
                            liftIO $ H.lookup tab e

  sMember (Entity e) = do ref <- mmKeys <$> get
                          S.member e <$> liftIO (readIORef ref)

  sDestroy (Entity e) = do MutableMap keys tab <- get
                           liftIO $ do H.delete tab e
                                       modifyIORef' keys (S.delete e)

  sStore (Entity e) (Just c) = do MutableMap keys tab <- get
                                  liftIO $ do H.insert tab e c
                                              modifyIORef' keys (S.insert e)

