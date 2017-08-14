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
newtype Elem   c = Elem   {unElem   :: SElem (Storage c)}
newtype Entity c = Entity {unEntity :: ID} deriving (Eq, Num, Show)

newtype Store  c = Store  {unStore  :: Storage c}
class w `Has` c where
  getStore :: Monad m => System w m (Store c)

type Valid w m c = (Has w c, Component c, SStorage m (Storage c))

newtype System w m a = System {unSystem :: ReaderT w m a} deriving (Functor, Monad, Applicative, MonadIO, MonadTrans)

runSystem :: System w m a -> w -> m a
runSystem sys = runReaderT (unSystem sys)

runWith :: w -> System w m a -> m a
runWith = flip runSystem

empty :: SStorage m (Storage c) => m (Store c)
empty = Store <$> sEmpty

slice :: forall w m c. Valid w m c => System w m (Slice c)
slice = do Store s :: Store c <- getStore
           fmap (Slice . fmap Entity) . lift $ sSlice s

isMember :: forall w m c. Valid w m c => Entity c -> System w m Bool
isMember (Entity e) = do Store s :: Store c <- getStore
                         lift $ sMember s e

retrieve :: forall w m c a. Valid w m c => Entity a -> System w m (Reads c)
retrieve (Entity e) = do Store s :: Store c <- getStore
                         fmap Reads . lift $ sRetrieve s e

store :: forall w m c a. Valid w m c => Writes c -> Entity a -> System w m ()
store (Writes w) (Entity e) = do Store s :: Store c <- getStore
                                 lift $ sStore s w e

destroy :: forall w m c. Valid w m c => Entity c -> System w m ()
destroy (Entity e) = do Store s :: Store c <- getStore
                        lift $ sDestroy s e

mapRW :: forall w m c. Valid w m c => (Elem c -> Elem c) -> System w m ()
mapRW f = do Store s :: Store c <- getStore
             lift $ sOver s (unElem . f . Elem)

forC :: forall w m c a. Valid w m c => (Elem c -> m a) -> System w m ()
forC fm = do Store s :: Store c <- getStore
             lift $ sForC s (fm . Elem)

union :: Slice s1 -> Slice s2 -> Slice ()
union (Slice s1) (Slice s2) = let set1 = S.fromList . fmap unEntity $ s1
                                  set2 = S.fromList . fmap unEntity $ s2
                               in Slice . fmap Entity . S.toList $ S.intersection set1 set2

instance (w `Has` a, w `Has` b) => w `Has` (a, b) where
  getStore = do Store sa :: Store a <- getStore
                Store sb :: Store b <- getStore
                return $ Store (sa, sb)
