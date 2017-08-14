{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, ConstraintKinds #-}
module Control.ECS.Core where

import qualified Data.IntSet as S

import Control.ECS.Storage
import Control.Monad.State
import Control.Monad.Reader

type Safe a = SSafeElem (Storage a)

newtype Slice  c = Slice  {toList   :: [Entity c]} deriving (Eq, Show)
newtype Reads  c = Reads  {unReads  :: Safe c}
newtype Writes c = Writes {unWrites :: Safe c}
newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Show)

newtype Store  c = Store  {unStore  :: Storage c}
class w `Has` c where
  getC :: Monad m => System w m (Store c)

type Valid w m c = (Has w c, Component c, SStorage m (Storage c))

newtype System w m a = System {unSystem :: ReaderT w m a} deriving (Functor, Monad, Applicative, MonadIO, MonadTrans)

runSystem :: System w m a -> w -> m a
runSystem sys = runReaderT (unSystem sys)

empty :: Valid w m c => System w m (Store c)
empty = System . lift $ Store <$> sEmpty

slice :: forall w m c. Valid w m c => System w m (Slice c)
slice = do Store s :: Store c <- getC
           fmap (Slice . fmap Entity) . lift $ sSlice s

isMember :: forall w m c. Valid w m c => Entity c -> System w m Bool
isMember (Entity e) = do Store s :: Store c <- getC
                         lift $ sMember s e

retrieve :: forall w m c a. Valid w m c => Entity a -> System w m (Reads c)
retrieve (Entity e) = do Store s :: Store c <- getC
                         fmap Reads . lift $ sRetrieve s e

store :: forall w m c a. Valid w m c => Entity a -> Writes c -> System w m ()
store (Entity e) (Writes w) = do Store s :: Store c <- getC
                                 lift $ sStore s w e

{--
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

--}
