{-# LANGUAGE Strict, ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

import Criterion
import qualified Criterion.Main as C
import Control.Monad

import Apecs as A
import Apecs.Stores
import Apecs.Util
import Apecs.Slice as S

import Linear

data Group w1 w2 = Group String (IO w1) (System w1 ()) (System w1 ()) (IO w2) (System w2 ()) (System w2 ())
toBench (Group name w1 i1 r1 w2 i2 r2) =
  bgroup name
    [ bgroup "naive" [bench "init" $ whnfIO (w1 >>= runSystem i1), bench "init and step" $ whnfIO (w2 >>= runSystem (i1 >> r1))]
    , bgroup "improved" [bench "init" $ whnfIO (w1 >>= runSystem i1), bench "init and step" $ whnfIO (w2 >>= runSystem (i1 >> r1))]
    ]

newtype W1 c = W1 {w1c1 :: (Storage c)}
instance Component c => Has (W1 c) c where getStore = System $ asks w1c1

data W2 a b = W2 { w2c1 :: Storage a , w2c2 :: Storage b }
instance (Component a, Component b) => Has (W2 a b) a where getStore = System $ asks w2c1
instance (Component a, Component b) => Has (W2 a b) b where getStore = System $ asks w2c2

--Explicit vs implicit map
newtype Counter

main :: IO ()
main = C.defaultMain [ ]
