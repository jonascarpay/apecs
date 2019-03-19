module Apecs.Callback where

import           Apecs.Core
import           Apecs.System (runSystem)

newtype Callback a w m r = Callback { unCallback :: a -> SystemT w m r }

instance Functor m => Functor (Callback a w m) where
  {-# INLINE fmap #-}
  fmap f (Callback cb) = Callback (fmap f . cb)

instance Applicative m => Applicative (Callback a w m) where
  {-# INLINE pure #-}
  pure = Callback . const . pure
  {-# INLINE (<*>) #-}
  Callback cba <*> Callback cbb = Callback $ \a -> cba a <*> cbb a

instance Monad m => Monad (Callback a w m) where
  {-# INLINE return #-}
  return = Callback . const . pure
  {-# INLINE (>>=) #-}
  Callback cba >>= fcbb = Callback $ \a -> cba a >>= ($a) . unCallback . fcbb

{-# INLINE runCallback #-}
runCallback :: Callback a w m r -> w -> (a -> m r)
runCallback (Callback cb) w a = runSystem (cb a) w

pureCB :: Applicative m => (a -> r) -> Callback a w m r
pureCB f = Callback $ pure . f
