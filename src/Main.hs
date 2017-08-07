import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

import Lib

data Vec2        = Vec Float Float deriving (Eq, Show)
newtype Position = Pos Vec2 deriving (Eq, Show)
newtype Velocity = Vel Vec2 deriving (Eq, Show)
data Enemy       = Enemy deriving (Eq, Show)
data MyWld       = MyWld { positions  :: !(M.IntMap Position)
                         , velocities :: !(M.IntMap Velocity)
                         , enemies    :: !S.IntSet
                         , eCounter   :: Int
                         } deriving (Eq, Show)

instance World MyWld where
  empty = MyWld mempty mempty mempty 0

  create cRep = do w <- get
                   undefined

  destroy = undefined

instance WComp MyWld Position where
  type Repr Position = Maybe Position

  store (Entity e) Nothing = do w <- get
                                put $ w { positions = M.delete e (positions w) }

  store (Entity e) (Just p) = do w <- get
                                 put $ w { positions = M.insert e p (positions w) }

  retrieve (Entity e) = M.lookup e . positions <$> get


instance WComp MyWld Velocity where
  type Repr Velocity = Maybe Velocity

  store (Entity e) Nothing = do w <- get
                                put $ w { velocities = M.delete e (velocities w) }

  store (Entity e) (Just v) = do w <- get
                                 put $ w { velocities = M.insert e v (velocities w) }

  retrieve (Entity e) = M.lookup e . velocities <$> get

instance WComp MyWld Enemy where
  type Repr Enemy = Bool
  store (Entity e) True = do w <- get
                             put $ w { enemies = S.insert e (enemies w) }
  store (Entity e) False = do w <- get
                              put $ w { enemies = S.delete e (enemies w) }

  retrieve (Entity e) = S.member e . enemies <$> get

simpleWorld = MyWld (M.fromList [ (1, Pos $ Vec 0 0)
                    , (2, Pos $ Vec 0 0)
                    , (3, Pos $ Vec 0 0)
                    ])
                    mempty
                    mempty

main = undefined
