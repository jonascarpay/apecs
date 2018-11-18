# apecs-stm
Experimental STM stores, allow apecs to be run in the STM monad.
```haskell
atomically . cmap $ \(Position p, Velocity v) -> Position (v+p)
```
Not included in the stack project until stm-containers (or similar) makes it into stackage.
