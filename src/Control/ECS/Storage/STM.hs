module Control.ECS.Storage.STM where

import Control.ECS.Storage
import Control.Concurrent.STM
import qualified STMContainers.Map as M
import qualified ListT as L

newtype STMMap c = STMMap {getSTMTable :: M.Map Int c}

instance SStorage STM (STMMap c) where
  type SSafeElem (STMMap c) = Maybe c
  type SElem     (STMMap c) = c

{--
  sEmpty = STMMap <$> M.new
  sSlice    (STMMap m) = fmap (Entity . fst) <$> (M.stream m >>= L.toList)
  sMember   (STMMap m) (Entity ety) = isJust <$> M.lookup h ety
  sDestroy  (STMMap m) (Entity ety) = M.delete h ety
  sRetrieve (STMMap m) (Entity ety) = M.lookup h ety
  sStore    h Nothing ety = sDestroy h ety
  sStore    (STMMap m) (Just x) (Entity ety) = M.insert h ety x
  sOver     (STMMap m) f = flip M.stream h $ \(k,x) -> M.insert h k (f x)
  sForC     (STMMap m) fm = flip M.stream h $ \(_,x) -> fm x

  {-# INLINE sEmpty #-}
  {-# INLINE sStore #-}
  {-# INLINE sOver #-}
  {-# INLINE sForC #-}
  {-# INLINE sSlice #-}
  {-# INLINE sMember #-}
  {-# INLINE sDestroy #-}
  {-# INLINE sRetrieve #-}
  --}
