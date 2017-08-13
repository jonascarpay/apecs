{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}

module Control.ECS (
  module Control.ECS.Core,
  module Control.ECS,
  module Control.ECS.Immutable,
) where

import qualified Data.IntSet as S

import Control.ECS.Core
import Control.ECS.Immutable
import Control.Monad.State

type Runtime a = SRuntime (Storage a)

newtype Slice  c = Slice  {toList   :: [Entity]} deriving (Eq, Show)
newtype Reads  c = Reads  {unReads  :: Runtime c}
newtype Writes c = Writes {unWrites :: Runtime c}
newtype Store  c = Store  {unStore  :: Storage c}

class w `Has` c where
  getC :: w -> Store c
  putC :: Store c -> w -> w

empty :: Component c => System s (Store c)
empty = Store <$> sEmpty

slice :: forall w c. (Has w c, Component c) => System w (Slice c)
slice = embed $ do Store s <- get
                   (a, s') <- runSystem sSlice s :: System (Store c) ([Entity], Storage c)
                   put (Store s')
                   return $ Slice a

retrieve :: forall w c. (Has w c, Component c) => Entity -> System w (Reads c)
retrieve e = embed $ do Store s <- get
                        (r, s') <- runSystem (sRetrieve e) s :: System (Store c) (Runtime c, Storage c)
                        put (Store s')
                        return $ Reads r

store :: forall w c. (Has w c, Component c) => Entity -> Writes c -> System w ()
store e (Writes w) = embed $ do Store s <- get
                                ((), s' :: Storage c) <- runSystem (sStore e w) s
                                put (Store s' :: Store c)

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = let set1 = S.fromList . fmap unEntity $ s1
                                  set2 = S.fromList . fmap unEntity $ s2
                               in Slice . fmap Entity . S.toList $ S.intersection set1 set2

embed :: Has w c => System (Store c) a -> System w a
embed sys = do w <- get
               (a, c') <- runSystem sys (getC w)
               put (putC c' w)
               return a

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getC w = let Store sa :: Store a = getC w
               Store sb :: Store b = getC w
            in Store (sa, sb)

  putC (Store (sa, sb)) = putC (Store sb :: Store b) . putC (Store sa :: Store a)

runWith :: s -> System s a -> System w (a, s)
runWith = flip runSystem

runWithIO :: s -> System s a -> IO (a, s)
runWithIO = flip runSystemIO

uninitialized :: w
uninitialized = error "Read uninitialized world state"

defaultMain :: System w a -> IO ()
defaultMain sys = void $ runSystemIO sys uninitialized

getEntityCount :: forall w. Has w EntityCounter => System w Int
getEntityCount = unEntityCounter . unReads <$> (retrieve undefined :: System w (Reads EntityCounter))

newEntity :: forall w. Has w EntityCounter => System w Entity
newEntity = do (Reads (EntityCounter c) :: Reads EntityCounter) <- retrieve nullEntity
               store nullEntity (Writes $ EntityCounter (c+1) :: Writes EntityCounter)
               return (Entity c)

newEntityWith :: (Component c, Has w EntityCounter, Has w c) => Writes c -> System w Entity
newEntityWith c = do e <- newEntity
                     store e c
                     return e

readGlobal :: (Monoid a, Has w (Global a)) => System w (Reads (Global a))
readGlobal = retrieve nullEntity

writeGlobal :: (Monoid a, Has w (Global a)) => System w (Reads (Global a))
writeGlobal = retrieve nullEntity

appendGlobal :: forall a w. (Monoid a, Has w (Global a)) => a -> System w (Reads (Global a))
appendGlobal a = do Reads m :: Reads (Global a) <- retrieve nullEntity
                    store nullEntity (Writes (m `mappend` a) :: Writes (Global a))
                    return (Reads m :: Reads (Global a))

mapReads :: forall w a b. (Has w a, Has w b, Component a, Component b) => (Reads a -> Writes b) -> System w ()
mapReads f = do sl :: Slice a <- slice
                forSlice_ sl $ \e -> retrieve e >>= store e . f

sliceReads :: (Component c, Has w c) => Slice a -> (Reads c -> System w b) -> System w ()
sliceReads sl sys = forM_ (toList sl) $ (>>= sys) . retrieve

forSlice_ :: Monad m => Slice c -> (Entity -> m b) -> m ()
forSlice_ sl = forM_ (toList sl)

