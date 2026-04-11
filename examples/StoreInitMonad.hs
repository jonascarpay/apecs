{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Apecs hiding (asks)
import Apecs.Core
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource
import System.IO (Handle, openTempFile, hClose)
import System.Directory (getTemporaryDirectory, removeFile)

newtype ComponentA = ComponentA Int

class HasValue env where
  getValue :: env -> Int

data ReaderStore = ReaderStore
  { componentsA :: [ComponentA],
    valueFromEnv :: Int
  }

instance Component ComponentA where
  type Storage ComponentA = ReaderStore

type instance Elem ReaderStore = ComponentA

instance (Monad m, HasValue env, MonadReader env m) => ExplInit m ReaderStore where
  explInit = do
    value <- asks getValue
    pure $ ReaderStore mempty value

newtype ComponentB = ComponentB Bool

data ResourceStore = ResourceStore
  { componentsB :: [ComponentB]
  , resource :: Handle
  }

instance Component ComponentB where
  type Storage ComponentB = ResourceStore

type instance Elem ResourceStore = ComponentB

instance (MonadResource m) => ExplInit m ResourceStore where
  explInit = do
    (_, (path, handle)) <- allocate setup teardown
    pure $ ResourceStore mempty handle
    where
      setup = do
        putStrLn "allocating resource"
        tmpDir <- getTemporaryDirectory
        openTempFile tmpDir "file-resource"
      teardown (path, handle) = do
        hClose handle
        removeFile path
        putStrLn "freed resource"

makeWorld "World" [''ComponentA, ''ComponentB]

data Env = Env
  { envValue :: Int
  }

instance HasValue Env where
  getValue = envValue

main :: IO ()
main = runResourceT $ runReaderT app $ Env 1

-- prints:
-- allocating resource
-- using resource
-- freed resource

app :: (MonadIO m, HasValue env, MonadReader env m, MonadResource m) => m ()
app = do
  world <- initWorld
  runWith world $ do
    liftIO $ putStrLn "using resource"


  
