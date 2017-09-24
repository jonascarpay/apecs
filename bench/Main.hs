{-# LANGUAGE Strict, ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

import Criterion
import qualified Criterion.Main as C
import Control.Monad

import Apecs as A
import Apecs.Stores
import Apecs.Util
import qualified Apecs.Slice as S

import Linear

data Group w1 w2 = Group
  { groupName :: String
  , naiveWorld :: IO w1
  , naiveInit :: System w1 ()
  , naiveRun  :: System w1 ()
  , improvedWorld :: IO w1
  , improvedInit :: System w1 ()
  , improvedRun  :: System w1 () }

toBench (Group name w1 i1 r1 w2 i2 r2) =
  bgroup name
    [ bgroup "naive"    [bench "init" $ whnfIO (w1 >>= runSystem i1), bench "init and run" $ whnfIO (w2 >>= runSystem (i1 >> r1))]
    , bgroup "improved" [bench "init" $ whnfIO (w1 >>= runSystem i1), bench "init and run" $ whnfIO (w2 >>= runSystem (i2 >> r2))]
    ]

data W1 c = W1 {w1c1 :: (Storage c), w1ec :: Storage EntityCounter}
instance Component c => Has (W1 c) c where getStore = System $ asks w1c1
instance Has (W1 c) EntityCounter where getStore = System $ asks w1ec

w1with args = W1 <$> initStoreWith args <*> initCounter
w1 = w1with ()
w2with a1 a2 = W2 <$> initStoreWith a1 <*> initStoreWith a2 <*> initCounter
w2 = w2with () ()

data W2 a b = W2 { w2c1 :: Storage a , w2c2 :: Storage b, w2ec :: Storage EntityCounter}
instance (Component a, Component b) => Has (W2 a b) a where getStore = System $ asks w2c1
instance (Component a, Component b) => Has (W2 a b) b where getStore = System $ asks w2c2
instance Has (W2 a b) EntityCounter where getStore = System $ asks w2ec

--Explicit vs implicit map
newtype Counter = Counter Int
instance Component Counter where type Storage Counter = Map Counter

mapExample = Group
  { groupName     = "Single component map"
  , naiveWorld    = w1 :: IO (W1 Counter)
  , naiveInit     = replicateM_ 10 (newEntity (Counter 0))
  , naiveRun      = owners >>= S.mapM_ (\(e :: Entity Counter) -> set e (Counter 1))
  , improvedWorld = w1 :: IO (W1 Counter)
  , improvedInit  = replicateM_ 10 (newEntity (Counter 0))
  , improvedRun   = cmap (const (Counter 1))
  }


-- ecs_bench
newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
instance Component ECSPos where type Storage ECSPos = Cache 10000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show)
instance Component ECSVel where type Storage ECSVel = Cache 1000 (Map ECSVel)

pvInit = do replicateM_ 1000 (newEntity (ECSPos 0, ECSVel 1))
            replicateM_ 9000 (newEntity (ECSPos 0))

pvStep = rmap $ \(ECSVel v, ECSPos p) -> ECSPos (p+v)

pvWorld :: IO (W2 ECSPos ECSVel)
pvWorld = w2

main :: IO ()
main = C.defaultMain
  [ bgroup "ecs_bench"
    [ bench "init" $ whnfIO (pvWorld >>= runSystem pvInit)
    , bench "step" $ whnfIO (pvWorld >>= runSystem (pvInit >> pvStep))
    ]
  , toBench mapExample
  ]
