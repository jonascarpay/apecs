{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Apecs hiding (asks)
import Apecs.Core
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource
import System.IO (Handle, openTempFile, hClose)
import System.Directory (getTemporaryDirectory, removeFile)
import Data.Data (Typeable, typeRep, typeRepTyCon)
import Data.Coerce

-- * Stores

-- | Global-like store, that returns its value from the initial environment
data ReaderStore c = ReaderStore
  { valueFromEnv :: Int
  }

class HasValue env where
  getValue :: env -> Int

instance (Monad m, HasValue env, MonadReader env m) => ExplInit m (ReaderStore c) where
  explInit = do
    value <- asks getValue
    pure $ ReaderStore value

instance (Monad m, Coercible Int c) => ExplGet m (ReaderStore c) where
  explGet :: ReaderStore c -> Int -> m c
  explGet ReaderStore{valueFromEnv} _ety = pure $ coerce valueFromEnv

  explExists :: ReaderStore c -> Int -> m Bool
  explExists _ _ = pure True

-- | Do-nothing store, showcasing custom constraints for initWorld
data ResourceStore c = ResourceStore
  { resource :: Handle
  }

type instance Elem (ResourceStore c) = c

instance (MonadResource m, Typeable c) => ExplInit m (ResourceStore c) where
  explInit = do
    (_, (path, handle)) <- allocate setup teardown
    liftIO . putStrLn $ "Got handle for " <> show path
    pure $ ResourceStore handle
    where
      compName = show (typeRepTyCon $ typeRep (Proxy :: Proxy c))
      setup = do
        putStrLn "allocating resource"
        tmpDir <- getTemporaryDirectory
        openTempFile tmpDir $ "apecs-ResourceStore-" <> compName <> "-"
      teardown (path, handle) = do
        hClose handle
        removeFile path
        putStrLn "freed resource"

-- * Components

newtype ComponentA = ComponentA Int
  deriving (Show)

instance Component ComponentA where
  type Storage ComponentA = ReaderStore ComponentA

type instance Elem (ReaderStore c) = c

newtype ComponentB = ComponentB Bool

instance Component ComponentB where
  type Storage ComponentB = ResourceStore ComponentB

-- * World & App

makeWorld "World" [''ComponentA, ''ComponentB]

data Env = Env
  { envValue :: Int
  }

instance HasValue Env where
  getValue = envValue

-- prints:
-- allocating resource
-- Got handle for "/tmp/apecs-ResourceStore-ComponentB-2799404-0"
-- running app
-- Entity 0 has ComponentA 62387
-- freed resource

main :: IO ()
main = runResourceT $ runReaderT app $ Env 62387

app :: (MonadIO m, HasValue env, MonadReader env m, MonadResource m) => m ()
app = do
  world <- initWorld
  runWith world $ do
    liftIO $ putStrLn "running app"
    e <- newEntity ()
    a :: ComponentA <- get e
    liftIO $ putStrLn $ "Entity " <> show (unEntity e) <> " has " <> show a
